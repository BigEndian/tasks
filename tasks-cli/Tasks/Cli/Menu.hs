module Tasks.Cli.Menu
   (
     Choice(..)
   , choiceKeys
   , choiceString
   , getChar'
   , promptAndRead
   , Menu(..)
   , padString
   , menuDisplay
   , menuChoose
   , menuRun
   , menuRunWhile
   , menuRunAccWhile
   , menuRunAccWhile1
   , menuRunModifying
   , menuMod
   , menuSubMod
   ) where

import System.Console.ANSI (clearScreen)
import System.Console.Readline (readline, addHistory)
import Control.Monad (liftM, when)
import Data.Maybe (isJust, fromJust, fromMaybe)
import System.IO (hSetEcho, stdin)

-- | A datatype used to represent a choice in a menu.
-- The characters represents which keys may be used to select
-- the choice, and the string is used to display the effect
-- of the choice.
data Choice = Choice String String deriving (Show, Read, Eq, Ord)

-- | Extract the keys from a choice
choiceKeys :: Choice -> String
choiceKeys (Choice cs _) = cs

-- | Extract the string from a choice
choiceString :: Choice -> String
choiceString (Choice _ s) = s

-- | getChar, but without echo
getChar' :: IO Char
getChar' =
   hSetEcho stdin False >>
   getChar >>= (\c -> hSetEcho stdin True >> return c)

-- | Present a prompt to the user and get a string back from them.
-- The boolean option determines whether to add the users input
-- to readline's history
promptAndRead :: String -> Bool -> IO String
promptAndRead prompt addh =
      hSetEcho stdin True >> keepReading >>= 
         (\ln -> hSetEcho stdin False >> return ln)
   where
      keepReading = readline prompt >>=
         maybe keepReading (\l -> when addh (addHistory l) >> return l)


data Menu r = Menu { menuTitle :: String
                   , menuChoices :: [Choice]
                   , menuHandler :: (Choice, Char) -> IO r
                   , menuSubmenuHandler :: Maybe (r -> IO r) -- Handle the result of a submenu
                   , menuSubmenus :: [Menu r] }

-- | Pad a string with char to an approximate length
-- Both sides surrounding the given string will be equal in length,
-- so the resultant string may not be of the exact specified length
padString :: Char -> Int -> String -> String
padString c l s = let midLength = length s in
   if midLength >= l then
      s
   else
      ps c (l - midLength - 2) (" "++s++" ")
   where
      ps c l s = if l <= 0 then s else ps c (l-2) ([c] ++ s ++ [c])


-- | Given an array of choices, and a character, find the first
-- choice which has the corresponding key in its choiceKeys array
-- May return Nothing if no choice matched
getCorrespondingChoice :: [Choice] -> Char -> Maybe Choice
getCorrespondingChoice [] _ = Nothing
getCorrespondingChoice choices ik =
      if null matches then
         Nothing
      else
         Just (head matches)
   where
      choiceMatches chr chc = chr `elem` choiceKeys chc
      matches = filter (choiceMatches ik) choices

-- | Display a menu's entries through the terminal
menuDisplay :: Menu r -> IO ()
menuDisplay menu@(Menu { menuTitle = mtit }) = do
      putStrLn mtit
      mapM_ (putStrLn . choiceString) (menuChoices menu)
      mapM_ (putStrLn . choiceString) (concatMap menuChoices $ menuSubmenus menu)

-- | Get a choice from a user for a menu.
-- If no valid key is supplied, Nothing will be returned
menuChooseOptional :: Menu r -> IO (Maybe (Choice, Char))
menuChooseOptional m@(Menu { menuChoices = choices }) = do
   ik <- getChar'
   let mchc = getCorrespondingChoice choices ik
   return (if isJust mchc then Just (fromJust mchc, ik) else Nothing)

-- | Get a choice from a user for a menu.
-- It will continue to prompt until it gets a valid choice.
-- It will return the choice as well as the key pressed
-- Error is called if choices is empty
menuChoose :: Menu r -> IO (Choice, Char)
menuChoose m@(Menu { menuChoices = choices }) = do
   mchc <- menuChooseOptional m
   maybe (menuChoose m) return mchc

-- | Given a set of menus, read a key from the user and return
-- a tuple containing whichever Menu first matched, the corresponding
-- choice, and the character inputted
menusChoose :: [Menu r] -> IO (Menu r, Choice, Char)
menusChoose menus = do
   ik <- getChar'
   let all_choices = map menuChoices menus
   let applied     = map (`getCorrespondingChoice` ik) all_choices
   let matches     = filter (isJust . snd) (zip [0..] applied)
   if null matches then
      menusChoose menus
      else
      let mtch = head matches in
         return (menus !! fst mtch, fromJust (snd mtch), ik)


-- | Display a menu, get a choice, then pass the resultant choice
-- to the menu's handler
menuRun :: Menu r -> IO r
menuRun m@(Menu { menuChoices  = mchcs
                , menuSubmenus = msbms
                , menuSubmenuHandler = msmh }) =
   case null msbms of
      True -> menuDisplay m >> menuChoose m >>= menuHandler m
      otherwise -> do clearScreen
                      menuDisplay m
                      (mmenu,chc,chr) <- menusChoose menus
                      res <- menuHandler mmenu (chc, chr)
                      handleRes res chc
   where
      menus = m:msbms
      handleRes r chc
         | chc `elem` mchcs = return r
         | otherwise        = fromMaybe return msmh r

menuRunWhile :: (r -> Bool) -> Menu r -> IO r
menuRunWhile f menu = do
   res <- menuRun menu;
   if not $ f res then
      return res
      else
         menuRunWhile f menu

menuRunAccWhile1 :: (r -> Bool) -> Menu r -> IO [r]
menuRunAccWhile1 f menu = do
   res <- menuRun menu;
   if not $ f res then
      return [res]
      else
      liftM (res:) $ menuRunAccWhile f menu

menuRunAccWhile :: (r -> Bool) -> Menu r -> IO [r]
menuRunAccWhile f menu = liftM init $ menuRunAccWhile1 f menu

menuRunModifying :: a -> (a -> r -> (a, Menu r)) -> (r -> Bool) -> Menu r -> IO r
menuRunModifying iacc gen test menu = do
   res' <- menuRun menu;
   if test res' then
      return res'
      else
         let (nacc,nmenu) = gen iacc res' in
            menuRunModifying nacc gen test nmenu

-- | Given a menu, create one which does the same thing,
-- but applies the given function f to the handler's return value.
-- This allows you to build a menu on top of another, one which
-- does something else with the result of the base menu.
-- If the menu has a submenu handler, it's removed
menuMod :: (r1 -> IO r2) -> Menu r1 -> Menu r2
menuMod f m@(Menu { menuHandler = mh
                  , menuSubmenus = msms }) =
      m { menuSubmenuHandler = Nothing
        , menuHandler = nmh mh
        , menuSubmenus = map (menuMod f) msms }
   where
      nmh mh tup = mh tup >>= f

-- | Given a menu, create one which applies a different handler to
-- any submenus which happen to be chosen when the new menu is ran.
menuSubMod :: (r -> IO r) -> Menu r -> Menu r
menuSubMod f m = m { menuSubmenuHandler = Just f }
