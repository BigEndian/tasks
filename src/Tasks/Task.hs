-- |
-- Module: Tasks.Task
--
-- This module contains the Task data type, used to represent a user's
-- task and what it relates to, how important it is, and whether or not
-- it's been completed.

module Tasks.Task(
     Task
   ) where

import Data.Binary
import Data.Maybe

import Tasks.Types

-- | The Task data type, storing a task's name, notes, priority, and
-- completion status.
data Task =
   Task { taskName :: TaskString
        , taskNotes :: TaskString
        , taskPriority :: Int
        , taskCompleted :: Bool } deriving (Read, Show)

-- | Construct a task given a name, optional notes,
-- and an optional priority value.
task :: String -> Maybe String -> Maybe Int -> Task
task tn mtns mtnp =
   Task { taskName = tsString tn
        , taskNotes = tsString $ fromMaybe "" mtns
        , taskPriority = fromMaybe 0 mtnp
        , taskCompleted = False }

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
