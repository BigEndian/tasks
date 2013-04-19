module Tasks.Cli.SimpleMenu
   (
     ChoiceSet
   , choice
   , (<+>)
--   , SimpleMenu
   ) where

import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust)

data ChoiceSet = Choice Char String | ChoiceSet [ChoiceSet] deriving Show

-- | Construct a choice given a string.
-- This method should be used instead of calling the Choice constructor
-- directly, as this will check for an ampersand and prepare the choice for
-- being displayed in a menu.
choice :: String -> ChoiceSet
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

choiceKey :: ChoiceSet -> Char
choiceKey (Choice c _) = c

choiceString :: ChoiceSet -> String
choiceString (Choice _ s) = s

infixl 1 <+>

(<+>) :: ChoiceSet -> ChoiceSet -> ChoiceSet 
c1@(Choice _ _) <+> c2@(Choice _ _) = ChoiceSet [c1, c2]
(ChoiceSet chcs1) <+> (ChoiceSet chcs2) = ChoiceSet (chcs1 ++ chcs2)
(ChoiceSet chcs) <+> chc@(Choice _ _) = ChoiceSet (chcs ++ [chc])
chc@(Choice _ _) <+> (ChoiceSet chcs) = ChoiceSet (chc:chcs)

data SimpleMenu r = SimpleMenu { smChoiceSet :: ChoiceSet
                               , smHandler :: ChoiceSet -> IO r }

exMenu = SimpleMenu { smChoiceSet =
                        choice "&List projects" <+>
                        choice "&Edit a project"
                    , smHandler = \_ -> return () }

displayMenu :: SimpleMenu r -> IO ()
displayMenu menu = mapM_ (putStrLn . choiceString) choices
   where
      choices = case (smChoiceSet menu) of
         c@(Choice _ _) -> [c]
         (ChoiceSet chcs)  -> chcs
