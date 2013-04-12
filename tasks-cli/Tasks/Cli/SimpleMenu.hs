module Tasks.Cli.SimpleMenu
   (
     ChoiceSet
   , choice
   , (<$>)
   , SimpleMenu
   ) where

data ChoiceSet k s = Choice k s | ChoiceSet [ChoiceSet k s] deriving Show

instance Functor (ChoiceSet k) where
   fmap f (Choice k s) = Choice k (f s)

choice :: k -> s -> ChoiceSet k s
choice = Choice

infixl 1 <$>

(<$>) :: ChoiceSet k s -> ChoiceSet k s -> ChoiceSet k s
c1@(Choice _ _) <$> c2@(Choice _ _) = ChoiceSet [c1, c2]
(ChoiceSet choices) <$> c@(Choice _ _) = ChoiceSet (choices ++ [c])
c@(Choice _ _) <$> (ChoiceSet choices) = ChoiceSet (c : choices)
(ChoiceSet as1) <$> (ChoiceSet as2) = ChoiceSet (as1 ++ as2)


data SimpleMenu r = SimpleMenu { smChoiceSet :: ChoiceSet Char String
                               , smHandler :: ChoiceSet Char String -> IO r }

exMenu = SimpleMenu { smChoiceSet =
                        choice 'L' "List projects" <$>
                        choice 'E' "Edit a project"
                    , smHandler = \_ -> return () }

presentMenu :: SimpleMenu r -> IO r
presentMenu menu =
      smHandler menu (Choice ' ' "")
   where
      choiceKey (Choice k _) = k
