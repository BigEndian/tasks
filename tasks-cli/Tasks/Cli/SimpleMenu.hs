module Tasks.Cli.SimpleMenu
   (
     Choice
   , choice
--   , SimpleMenu
   ) where

import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust)

data Choice = Choice Char String

-- | Construct a choice given a string.
-- This method should be used instead of calling the Choice constructor
-- directly, as this will check for an ampersand and prepare the choice for
-- being displayed in a menu.
choice :: String -> Choice
choice text
      | isJust midx = let (l,r) = splitAt idx text in
         Choice (head $ drop 1 r) (l ++ parenthesize r)
      | otherwise   =
         error $ "Text must have an ampersand to determine which " ++
                 "character to use to select this choice"
   where
      midx = elemIndex '&' text
      idx = fromJust midx
      parenthesize s = '(' : (s!!1) : ')' : drop 2 s

choiceKey :: Choice -> Char
choiceKey (Choice c _) = c

choiceString :: Choice -> String
choiceString (Choice _ s) = s

data SimpleMenu r = SimpleMenu { smChoiceSet :: [Choice]
                               , smHandler :: Choice -> IO r }

exMenu = SimpleMenu { smChoiceSet =
                        choice "&List projects" :
                        choice "&Edit a project" : []
                    , smHandler = \_ -> return () }

displayMenu :: SimpleMenu r -> IO ()
displayMenu menu = mapM_ (putStrLn . choiceString) (smChoiceSet menu)
