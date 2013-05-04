module Tasks.Cli.Menu
   (
     Choice(..)
   , choiceKeys
   , choiceString
   , Menu(..)
   , menuDisplay
   , menuChoose
   , menuRun
   , menuMod
   , menuSubMod
   ) where

import Data.Char (isAlpha, toUpper, toLower)
import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust, fromMaybe)
import System.IO (hSetEcho, stdin)

-- | A datatype used to represent a choice in a menu.
-- The characters represents which keys may be used to select
-- the choice, and the string is used to display the effect
-- of the choice.
data Choice = Choice String String deriving Show

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

data Menu r = Menu { menuChoices :: [Choice]
                   , menuHandler :: (Choice, Char) -> IO r
                   , menuSubmenuHandler :: Maybe (r -> IO r) -- Handle the result of a submenu
                   , menuSubmenus :: [Menu r] }

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
menuDisplay menu = mapM_ (putStrLn . choiceString) (menuChoices menu)

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
   maybe (putStrLn "Invalid choice" >> menuChoose m) return mchc

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
      putStr "Invalid choice" >> menusChoose menus
      else
      let mtch = head matches in
         return (menus !! fst mtch, fromJust (snd mtch), ik)
   

-- | Display a menu, get a choice, then pass the resultant choice
-- to the menu's handler
menuRun :: Menu r -> IO r
menuRun m@(Menu { menuSubmenus = msbms
                , menuSubmenuHandler = msmh }) =
   if null msbms then
      menuDisplay m >> menuChoose m >>= menuHandler m
   else
      mapM_ menuDisplay menus >>
      menusChoose menus >>= (\(mmenu,chc,chr) ->
         menuHandler mmenu (chc, chr)) >>=
         fromMaybe return msmh
   where
      menus = m:msbms

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

-- | Givne a menu, create one which applies a different handler to
-- any submenus which happen to be chosen when the new menu is ran.
menuSubMod :: (r -> IO r) -> Menu r -> Menu r
menuSubMod f m = m { menuSubmenuHandler = Just f }
