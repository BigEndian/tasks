-- |
-- Module: Tasks.Task
--
-- This module contains the Task data type, used to represent a user's
-- task and what it relates to, how important it is, and whether or not
-- it's been completed.

module Tasks.Task(
     Task(..)
   , task
   ) where

import Data.Binary
import Data.Maybe
import Data.DateTime
import qualified Data.ByteString as B

import Tasks.Types

-- | The Task data type, storing a task's name, notes, priority, and
-- completion status.
data Task =
   Task { taskName :: B.ByteString
        , taskNotes :: B.ByteString
        , taskPriority :: Int
        , taskCompleted :: Bool 
        , taskDue :: Maybe DateTime } deriving (Read, Show)

-- | Construct a task given a name, optional notes,
-- and an optional priority value.
task :: String -> Maybe String -> Maybe Int -> Maybe DateTime -> Task
task tn mtns mtnp mtd =
   Task { taskName = bs tn
        , taskNotes = bs $ fromMaybe "" mtns
        , taskPriority = fromMaybe 0 mtnp
        , taskCompleted = False
        , taskDue = mtd }

instance Eq Task where
   (==) (Task { taskName = tt1 })
        (Task { taskName = tt2 }) = tt1 == tt2

instance Binary Task where
   get = do
      tn <- get :: Get B.ByteString
      tns <- get :: Get B.ByteString
      tp <- get :: Get Int
      tb <- get :: Get Bool
      mtd <- get :: Get (Maybe DateTime)
      return Task { taskName = tn
                  , taskNotes = tns
                  , taskPriority = tp
                  , taskCompleted = tb
                  , taskDue = mtd }

   put t = do
      put $ taskName t
      put $ taskNotes t
      put $ taskPriority t
      put $ taskCompleted t
