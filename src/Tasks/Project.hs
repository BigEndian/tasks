-- |
-- This module contains the Project type.
-- Like the Task type, a Project has a name and may have notes,
-- but it also contains zero or more task entries.
-- Like task, it implements the Binary instance.
module Tasks.Project
   (
     Project(..) -- The @Project@ type
   , projectHasTasks
   , projectHasTask
   , projectAddTask
   , project
   ) where


import Control.Monad
import Data.Maybe
import Data.Binary
import Data.List(elem, delete, null)
import qualified Data.ByteString as B

import Tasks.Task
import Tasks.Types

-- | The project type, containing the name, notes, and tasks fields
data Project = Project { projectName :: B.ByteString
                       , projectNotes :: Maybe B.ByteString
                       , projectTasks :: Maybe [Task] }

instance Eq Project where
   (==) (Project { projectName = pn1 })
        (Project { projectName = pn2 }) = pn1 == pn2

instance Binary Project where
   get = do
      (pn, pns, pts) <- get :: Get (B.ByteString, Maybe B.ByteString, Maybe [Task])
      return Project { projectName = pn
                     , projectNotes = pns
                     , projectTasks = pts }

   put p =
      put ( projectName p
          , projectNotes p
          , projectTasks p)


instance Show Project where
   showsPrec _ p = ((bsToString $ projectName p)++)


-- | Convenience method to determine whether a project has any
-- tasks whatsoever
projectHasTasks :: Project -> Bool
projectHasTasks (Project { projectTasks = pts }) = case pts of
   Nothing      -> False
   (Just tasks) -> not (null tasks)

-- | Check whether a project contains a particular task
projectHasTask :: Project -> Task -> Bool
projectHasTask (Project { projectTasks = pts }) t
   | isJust pts = t `elem` fromJust pts
   | otherwise  = False

-- | Add a task to a given project, returning a tuple containing the (possibly)
-- modified project, and a boolean indicating whether the task was added.
projectAddTask :: Project -> Task -> (Project, Bool)
projectAddTask p@(Project { projectTasks = pts }) t =
   if projectHasTask p t then
      (p, False)
   else 
      let newTasks = Just $ fromMaybe [] pts ++ [t] in
         (p { projectTasks = newTasks }, True)

-- | Remove a task from a given project, returning a tuple containing the (possibly)
-- modified project, and a boolean indicating whether the task was removed.
projectRemoveTask :: Project -> Task -> (Project, Bool)
projectRemoveTask p@(Project { projectTasks = pts }) t =
   if not $ projectHasTask p t then
      (p, False)
   else
      let newTasks = Just $ delete t (fromJust pts) in
         (p { projectTasks = newTasks }, True)

-- | Easily construct a project, given a name, notes, and tasks
project :: String -> Maybe String -> Maybe [Task] -> Project
project pnm mpnts mpts =
   Project { projectName = bs pnm
           , projectNotes = fmpnts
           , projectTasks = mpts }
   where
      fmpnts = case mpnts of
         (Just str) -> Just . bs $ str
         Nothing    -> Nothing
