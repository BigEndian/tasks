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

import Control.Monad(liftM)
import Data.Binary
import Data.Maybe
import Data.DateTime
import qualified Data.ByteString as B

import Tasks.Types

data TaskPriority = Low | Medium | High deriving (Show, Read, Eq, Ord, Bounded, Enum)

instance Binary TaskPriority where
   get = liftM toEnum get
   put = put . fromEnum

-- | A type which encapsulates all metadata for a task
data TaskMetadata =
   TaskMetadata { tmdNotes :: Maybe B.ByteString
                , tmdPriority :: TaskPriority
                , tmdCompleted :: Bool
                , tmdDue :: Maybe DateTime }

instance Binary TaskMetadata where
   get = do
      mtmdn <- get :: Get (Maybe B.ByteString)
      tmdp  <- get :: Get TaskPriority
      tmdc  <- get :: Get Bool
      mtmdd <- get :: Get (Maybe DateTime)
      return TaskMetadata { tmdNotes = mtmdn
                          , tmdPriority = tmdp
                          , tmdCompleted = tmdc
                          , tmdDue = mtmdd }

   put tmd =
      put ( tmdNotes tmd
          , tmdPriority tmd
          , tmdCompleted tmd
          , tmdDue tmd )

-- | The Task data type, storing a task's name, notes, priority, and
-- completion status.
data Task =
   Task { taskName :: B.ByteString
        , taskMetadata :: TaskMetadata }


taskNotes = tmdNotes . taskMetadata

taskPriority = tmdPriority . taskMetadata

taskCompleted = tmdCompleted . taskMetadata

taskDue = tmdDue . taskMetadata


instance Eq Task where
   (==) (Task { taskName = tt1 })
        (Task { taskName = tt2 }) = tt1 == tt2

instance Binary Task where
   get = do
      tn <- get :: Get B.ByteString
      tmd <- get :: Get TaskMetadata
      return Task { taskName = tn
                  , taskMetadata = tmd }

   put t = do
      put ( taskName t
          , taskMetadata t )

-- | Construct a task given a name, optional notes,
-- an optional priority value, and an optional datetime at which it is due.
task :: String -> Maybe String -> Maybe TaskPriority -> Maybe DateTime -> Task
task tn mtns mtnp mtd =
   Task { taskName = bs tn
        , taskMetadata = tmd }
   where
      tmd = TaskMetadata { tmdNotes = fmap bs mtns
                         , tmdPriority = fromMaybe Low mtnp
                         , tmdCompleted = False
                         , tmdDue = mtd }

{-
 - This section just contains some examples, useful for playing around with
 - tasks (aka testing them). I cba to use a testing framework atm
 -}

exTask1 = task "Example Task 1" (Just "An example task") (Just Medium) Nothing
exTask2 = task "Example Task 2" (Just "An example task") (Just High) Nothing
exTasks = [exTask1, exTask2]
