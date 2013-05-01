module Tasks.Cli.Menu
   (
     Choice(..)
   , choice
   , choiceKeys
   , choiceString
   , Menu(..)
   , menuDisplay
   , menuChoose
   , menuRun
   ) where

import Control.Monad (liftM)
import Data.Char (isAlpha, toUpper, toLower)
import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust)
import System.IO (hSetEcho, stdin)

-- | A datatype used to represent a choice in a menu.
-- The characters represents which keys may be used to select
-- the choice, and the string is used to display the effect
-- of the choice.
data Choice = Choice String String deriving Show

-- | Construct a choice given a string.
-- This method should be used instead of calling the Choice constructor
-- directly, as this will check for an ampersand and prepare the choice for
-- being displayed in a menu.
--
-- > choice "&List projects" == Choice "Ll" "(L)ist projects"
choice :: String -> Choice
choice text
      | isJust midx = let (l,r) = splitAt idx text in
         Choice (uandl $ r !! 1) (l ++ parenthesize r)
      | otherwise   =
         error $ "Text must have an ampersand to determine which " ++
                 "character to use to select this choice"
   where
      midx = elemIndex '&' text
      idx = fromJust midx
      uandl c = if isAlpha c then [toUpper c, toLower c] else [c]
      parenthesize s = '(' : (s!!1) : ')' : drop 2 s

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

data Menu i r = Menu { menuChoices :: [Choice]
                     , menuInternal :: i
                     , menuHandler :: i -> (Choice, Char) -> IO r
                     , menuSubmenus :: [Menu i r] }

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
menuDisplay :: Menu i r -> IO ()
menuDisplay menu = mapM_ (putStrLn . choiceString) (menuChoices menu)

-- | Get a choice from a user for a menu.
-- If no valid key is supplied, Nothing will be returned
menuChooseOptional :: Menu i r -> IO (Maybe (Choice, Char))
menuChooseOptional m@(Menu { menuChoices = choices }) = do
   ik <- getChar'
   let mchc = getCorrespondingChoice choices ik
   return (if isJust mchc then Just (fromJust mchc, ik) else Nothing)

-- | Get a choice from a user for a menu.
-- It will continue to prompt until it gets a valid choice.
-- It will return the choice as well as the key pressed
-- Error is called if choices is empty
menuChoose :: Menu i r -> IO (Choice, Char)
menuChoose m@(Menu { menuChoices = choices }) = do
   mchc <- menuChooseOptional m
   maybe (putStrLn "Invalid choice" >> menuChoose m) return mchc

-- | Given a set of menus, read a key from the user and return
-- a tuple containing whichever Menu first matched, the corresponding
-- choice, and the character inputted
menusChoose :: [Menu i r] -> IO (Menu i r, Choice, Char)
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
menuRun :: Menu i r -> IO r
menuRun m@(Menu { menuSubmenus = msbms }) =
   if null msbms then
      menuDisplay m >> menuChoose m >>= menuHandler m (menuInternal m)
   else
      mapM_ menuDisplay menus >>
      menusChoose menus >>= (\(mmenu,chc,chr) ->
         menuHandler mmenu (menuInternal mmenu) (chc, chr))
   where
      menus = m:msbms
