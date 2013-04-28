-- |
-- Module: Tasks.Task
--
-- This module contains the Task data type, used to represent a user's
-- task 

module Tasks.Task
   (
     Task(..)

   -- Task accessor functions
   , taskNotes
   , taskPriority
   , taskCompleted
   , taskDue

   -- Export for testing purposes
   , exTask1, exTask2, exTasks
   ) where

import Data.Binary
import Data.DateTime
import Data.Maybe
import qualified Data.ByteString as B

import Tasks.Types


-- | The Task data type, storing a task's name, notes, priority,
-- completion status, and due date.
data Task =
   Task { taskName :: B.ByteString
        , taskMetadata :: Metadata } deriving (Show, Read)


-- Functions to quickly access task metadata
taskNotes = mdNotes . taskMetadata

taskPriority = mdPriority . taskMetadata

taskCompleted = mdCompleted . taskMetadata

taskDue = mdDue . taskMetadata

instance Eq Task where
   (==) (Task { taskName = tt1 })
        (Task { taskName = tt2 }) = tt1 == tt2

instance Ord Task where
   t1 >= t2 = (taskPriority t1) >= (taskPriority t2)

instance Binary Task where
   get = do
      tn <- get :: Get B.ByteString
      tmd <- get :: Get Metadata
      return Task { taskName = tn
                  , taskMetadata = tmd }

   put t = do
      put ( taskName t
          , taskMetadata t )

-- | Construct a task given a name, optional notes,
-- an optional priority value, and an optional datetime at which it is due.
task :: String -> Maybe String -> Maybe Priority -> Maybe DateTime -> Task
task tn mtns mtnp mtd =
   Task { taskName = bs tn
        , taskMetadata = metadata mtns mtnp (Just False) mtd }

{-
 - This section just contains some examples, useful for playing around with
 - tasks (aka testing them). I cba to use a testing framework atm
 -}

exTask1 = task "Example Task 1" (Just "An example task") (Just Medium) Nothing
exTask2 = task "Example Task 2" (Just "An example task") (Just High) Nothing
exTasks = [exTask1, exTask2]
