module Tasks.Task (Task, task) where
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import qualified Text.Read as R

data Task = Task { taskTitle :: B.ByteString, taskNotes :: Maybe B.ByteString, taskPriority :: Int }

task :: String -> Maybe String -> Task
task t ns = Task { taskTitle = B.pack $ t, taskNotes = tnsbs, taskPriority = 0 } 
               where
                  tnsbs = if isJust $ ns then (Just . B.pack . fromJust) ns
                          else Nothing :: Maybe B.ByteString

instance Show Task where
   show t = let f = B.unpack $ taskTitle t in 
               case (taskNotes t) of
                                    (Just nbs) -> f ++ " Notes: " ++ (B.unpack nbs)
                                    Nothing -> f

instance Read Task where
   readsPrec p s = [(Task { taskTitle = B.pack $ s, taskNotes = Just $ B.pack "", taskPriority = 0 }, s)]
   readListPrec = R.readListPrecDefault


instance Eq Task where
   (==) (Task t1t _ t1p) (Task t2t _ t2p) = and [t1t == t2t, t1p == t2p]


instance Ord Task where
   (<=) (Task _ _ t1p) (Task _ _ t2p) = (<=) t1p t2p
