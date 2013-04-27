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
import System.IO (hGetEcho, hSetEcho, stdin)

-- | A datatype used to represent a choice in a menu.
-- The characters represents which keys may be used to select
-- the choice, and the string is used to display the effect
-- of the choice.
data Choice = Choice [Char] String deriving Show

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
choiceKeys :: Choice -> [Char]
choiceKeys (Choice cs _) = cs

-- | Extract the string from a choice
choiceString :: Choice -> String
choiceString (Choice _ s) = s

data Menu r = Menu { menuChoices :: [Choice]
                   , menuHandler :: Choice -> IO r }

-- | Given an array of choices, and a character, find the first
-- choice which has the corresponding key in its choiceKeys array
-- May return Nothing if no choice matched
getCorrespondingChoice :: [Choice] -> Char -> Maybe Choice
getCorrespondingChoice [] _ = Nothing
getCorrespondingChoice choices ik = do
      if length matches == 0 then
         Nothing
      else
         Just (head matches)
   where
      choiceMatches chr chc = any (==chr) (choiceKeys chc)
      matches = filter (choiceMatches ik) choices

-- | Display a menu's entries through the terminal
menuDisplay :: Menu r -> IO ()
menuDisplay menu = mapM_ (putStrLn . choiceString) (menuChoices menu)

-- | Get a choice from a user for a menu
-- It will continue to prompt until it gets a valid choice
-- Error is called if choices is empty
menuChoose :: Menu r -> IO Choice
menuChoose m@(Menu { menuChoices = choices }) = do
   old_echo <- hGetEcho stdin
   hSetEcho stdin False
   ik <- getChar
   mchc <- return $ getCorrespondingChoice choices ik
   hSetEcho stdin old_echo
   if isJust mchc then
      return $ fromJust mchc
   else
      putStrLn "Invalid choice." >> menuChoose m

-- | Display a menu, get a choice, then pass the resultant choice
-- to the menu's handler
menuRun :: Menu r -> IO r
menuRun menu = menuDisplay menu >> menuChoose menu >>= menuHandler menu
