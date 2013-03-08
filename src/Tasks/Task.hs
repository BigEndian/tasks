module Tasks.Task(Task) where

import Data.Binary
import Tasks.Types

data Task =
   Task { taskName :: TaskString
        , taskNotes :: TaskString
        , taskPriority :: Int
        , taskCompleted :: Bool } deriving (Read, Show)

instance Eq Task where
   (==) (Task { taskName = tt1 })
        (Task { taskName = tt2 }) = tt1 == tt2

instance Binary Task where
   get = do
      tn <- get :: Get TaskString
      tns <- get :: Get TaskString
      tp <- get :: Get Int
      tb <- get :: Get Bool
      return Task { taskName = tn
                  , taskNotes = tns
                  , taskPriority = tp
                  , taskCompleted = tb }

   put t = do
      put $ taskName t
      put $ taskNotes t
      put $ taskPriority t
      put $ taskCompleted t
