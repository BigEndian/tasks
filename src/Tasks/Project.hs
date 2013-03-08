-- |
-- This module contains the Project type.
-- Like the Task type, a Project has a name and may have notes,
-- but it also contains zero or more task entries.
-- Like task, it implements the Binary instance.
module Tasks.Project
   (
     Project -- The @Project@ type
   , projectHasTask
   , projectAddTask
   , project
   ) where


import Control.Monad
import Data.Maybe
import Data.Binary

import Tasks.Task
import Tasks.Types(TaskString, tsString)

-- | The project type, containing the name, notes, and tasks fields
data Project = Project { projectName :: TaskString
                       , projectNotes :: TaskString
                       , projectTasks :: [Task] } deriving (Show, Read)

instance Eq Project where
   (==) (Project { projectName = pn1 })
        (Project { projectName = pn2 }) = pn1 == pn2

instance Binary Project where
   get = do
      pn <- get :: Get TaskString
      pns <- get :: Get TaskString
      pts <- get :: Get [Task]
      return Project { projectName = pn
                     , projectNotes = pns
                     , projectTasks = pts }

   put p = do
      put (projectName p)
      put (projectNotes p)
      put (projectTasks p)

-- | Check whether a project contains a particular task
projectHasTask :: Project -> Task -> Bool
projectHasTask p t = t `elem` projectTasks p

-- | Add a task to a given project, returning a tuple containing the (possibly)
-- modified project, and a boolean indicating whether the task was added.
projectAddTask :: Project -> Task -> (Project, Bool)
projectAddTask p t =
   if projectHasTask p t then
      (p, False)
   else 
      (p { projectTasks = projectTasks p ++ [t] }, True)

-- | Easily construct a project, given a name, notes, and tasks
project :: String -> Maybe String -> Maybe [Task] -> Project
project nm ns tsks =
   Project { projectName = nmbws
           , projectNotes = nsbws
           , projectTasks = fromMaybe [] tsks }
   where
      nmbws = tsString nm
      nsbws = tsString (fromMaybe "" ns)
