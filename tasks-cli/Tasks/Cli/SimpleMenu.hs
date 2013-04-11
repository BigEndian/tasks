module Tasks.Cli.SimpleMenu
   (
     ChoiceSet
   , choice
   , (<$>)
   , SimpleMenu
   ) where

data ChoiceSet = Choice String | ChoiceSet [ChoiceSet] deriving Show

choice :: String -> ChoiceSet
choice = Choice

infixl 1 <$>
(<$>) :: ChoiceSet -> ChoiceSet -> ChoiceSet
c1@(Choice s1) <$> c2@(Choice s2) = ChoiceSet [c1, c2]
cset@(ChoiceSet csets)  <$> c@(Choice s) = ChoiceSet (csets ++ [c])
c@(Choice s) <$> cset@(ChoiceSet csets) = ChoiceSet (c : csets)
cset1@(ChoiceSet csets1) <$> cset2@(ChoiceSet csets2) = ChoiceSet (csets1 ++ csets2)

data SimpleMenu r = SimpleMenu { smChoiceSet :: ChoiceSet
                               , smHandler :: IO r }
