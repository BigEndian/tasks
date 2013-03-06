module Tasks.Task (Task, TaskString) where

import qualified Data.ByteString as BW
import Data.Maybe
import Data.Word(Word8(..))
import Data.Binary
import Data.Char(chr, ord)
import Control.Monad

import Tasks.Serialization

newtype TaskString = TaskString BW.ByteString deriving (Read, Show)

stringToTaskString :: String -> TaskString
stringToTaskString = TaskString . stringToWByteString

taskStringToWByteString :: TaskString -> BW.ByteString
taskStringToWByteString (TaskString bws) = bws

word8sToTaskString :: [Word8] -> TaskString
word8sToTaskString = TaskString . BW.pack

taskStringIsEmpty :: TaskString -> Bool
taskStringIsEmpty (TaskString bws) =
   bws == bwse || (bwsws !! 0) == 0
   where
      bwse = BW.pack []
      bwsws = BW.unpack bws

instance Binary TaskString where
   get = do
            (return . word8sToTaskString . init) =<< readWord8sUntil 0
         where
            readWord8sUntil :: Word8 -> Get [Word8]
            readWord8sUntil val = do
               w8 <- getWord8
               if w8 == val then
                  return $ [w8]
               else
                  (return . (w8:)) =<< (readWord8sUntil val)

   put (TaskString bws) = mapM_ putWord8 $ (BW.unpack bws) ++ [0]

data Task = 
   Task { taskTitle :: TaskString, taskNotes :: TaskString, taskPriority :: Int }
      deriving (Read, Show)


instance Binary Task where
   get = do
            tt <- get :: Get TaskString
            tn <- get :: Get TaskString
            tp <- get :: Get Int
            return Task { taskTitle = tt, taskNotes = tn, taskPriority = tp }

   put t = do
            put $ taskTitle t
            put $ taskNotes t
            put $ taskPriority t


exTaskTitle = stringToTaskString "Do the dishes"
exTaskNotes = stringToTaskString "Must be done by 12:00 today"
exTaskPriority = 0
encTaskTitle = encode exTaskTitle
decTaskTitle = decode encTaskTitle :: TaskString

exTask = Task { taskTitle = exTaskTitle,
                taskNotes = exTaskNotes,
                taskPriority = exTaskPriority }

encTask = encode exTask
decTask = decode encTask :: Task
