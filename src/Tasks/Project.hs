-- |
-- Module: Tasks.Project
--
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
   , projectRemoveTask
   , project

   -- Helper functions
   , projectNotes
   , projectPriority
   , projectCompleted
   , projectDue

   , projectEditNotes
   , projectEditPriority
   , projectEditCompleted
   , projectEditDue

   -- Debugging
   , exProject
   ) where


import Data.DateTime
import Data.Binary
import Data.List (delete)
import qualified Data.ByteString as B

import Tasks.Task
import Tasks.Types

-- | The project type, containing the name, notes, and tasks fields
data Project = Project { projectName :: B.ByteString
                       , projectTasks :: [Task]
                       , projectMetadata :: Metadata } deriving (Read, Show)

-- Helper functions
projectNotes = mdNotes . projectMetadata

projectPriority = mdPriority . projectMetadata

projectCompleted = mdCompleted . projectMetadata

projectDue = mdDue . projectMetadata


-- | Edit a given project's notes
projectEditNotes :: Project -> Maybe B.ByteString -> Project
projectEditNotes p@(Project { projectMetadata = pmd }) mnts =
   p { projectMetadata = mdEditNotes pmd mnts }

-- | Edit a given project's priority
projectEditPriority :: Project -> Priority -> Project
projectEditPriority p@(Project { projectMetadata = pmd }) pr =
   p { projectMetadata = mdEditPriority pmd pr }

-- | Edit a given project's completion status
projectEditCompleted :: Project -> Bool -> Project
projectEditCompleted p@(Project { projectMetadata = pmd }) c =
   p { projectMetadata = mdEditCompleted pmd c }

-- | Edit a given project's due date
projectEditDue :: Project -> Maybe DateTime -> Project
projectEditDue p@(Project { projectMetadata = pmd }) mdd =
   p { projectMetadata = mdEditDue pmd mdd }

instance Eq Project where
   (==) (Project { projectName = pn1 })
        (Project { projectName = pn2 }) = pn1 == pn2

instance Binary Project where
   get = do
      (pn, ptsks, pmd) <- get :: Get (B.ByteString, [Task], Metadata)
      return Project { projectName = pn
                     , projectTasks = ptsks
                     , projectMetadata = pmd }

   put p =
      put ( projectName p
          , projectTasks p
          , projectMetadata p )

-- | Convenience method to determine whether a project has any
-- tasks whatsoever
projectHasTasks :: Project -> Bool
projectHasTasks (Project { projectTasks = pts }) = not (null pts)

-- | Check whether a project contains a particular task
projectHasTask :: Project -> Task -> Bool
projectHasTask (Project { projectTasks = pts }) t = t `elem` pts

-- | Add a task to a given project, returning a tuple containing the (possibly)
-- modified project, and a boolean indicating whether the task was added.
projectAddTask :: Project -> Task -> (Project, Bool)
projectAddTask p@(Project { projectTasks = pts }) t =
   if projectHasTask p t then
      (p, False)
   else
      let newTasks = pts ++ [t] in
         (p { projectTasks = newTasks }, True)

-- | Remove a task from a given project, returning a tuple containing the (possibly)
-- modified project, and a boolean indicating whether the task was removed.
projectRemoveTask :: Project -> Task -> (Project, Bool)
projectRemoveTask p@(Project { projectTasks = pts }) t =
   if not $ projectHasTask p t then
      (p, False)
   else
      let newTasks = delete t pts in
         (p { projectTasks = newTasks }, True)

-- | Easily construct a project, given a name, notes, priority,
-- due date, and tasks.
project :: String -> Maybe String -> Maybe Priority -> Maybe DateTime -> [Task] -> Project
project pnm mpnts mp md tsks =
   Project { projectName = bs pnm
           , projectTasks = tsks
           , projectMetadata = metadata mpnts mp (Just False) md }

exProject = project "Example Project"
                    (Just "A sample project")
                    (Just Medium)
                    Nothing
                    exTasks
