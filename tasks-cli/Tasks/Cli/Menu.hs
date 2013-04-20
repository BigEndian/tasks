module Tasks.Cli.Menu
   (
     Choice(..)
   , choice
   , choiceKey
   , choiceString
   , Menu(..)
   , menuDisplay
   , menuChoose
   ) where

import Control.Monad (liftM)
import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust)

-- | A datatype used to represent a choice in a menu.
-- The character represents which key should be used to select
-- the choice, and the string is used to display the effect
-- of the choice.
data Choice = Choice Char String deriving Show

-- | Construct a choice given a string.
-- This method should be used instead of calling the Choice constructor
-- directly, as this will check for an ampersand and prepare the choice for
-- being displayed in a menu.
--
-- > choice "&List projects" == Choice 'L' "(L)ist projects"
choice :: String -> Choice
choice text
      | isJust midx = let (l,r) = splitAt idx text in
         Choice (r !! 1) (l ++ parenthesize r)
      | otherwise   =
         error $ "Text must have an ampersand to determine which " ++
                 "character to use to select this choice"
   where
      midx = elemIndex '&' text
      idx = fromJust midx
      parenthesize s = '(' : (s!!1) : ')' : drop 2 s

-- | Extract the key from a choice
choiceKey :: Choice -> Char
choiceKey (Choice c _) = c

-- | Extract the string from a choice
choiceString :: Choice -> String
choiceString (Choice _ s) = s

data Menu r = Menu { menuChoices :: [Choice]
                   , menuHandler :: Choice -> IO r }

-- | Display a menu's entries through the terminal
menuDisplay :: Menu r -> IO ()
menuDisplay menu = mapM_ (putStrLn . choiceString) (menuChoices menu)

menuChoose :: Menu r -> IO Choice
menuChoose menu = do
   mchidx <- liftM (`elemIndex` characters) getChar -- IO (Maybe Int)
   if isJust mchidx then
      return $ choices !! fromJust mchidx
   else
      putStrLn "Invalid choice" >> menuChoose menu
   where
      choices = menuChoices menu
      characters = map choiceKey choices
