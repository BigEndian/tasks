module Tasks.Cli.Main where

import Control.Monad
import Data.Binary(decodeFile, encodeFile)
import System.FilePath
import System.Directory(getHomeDirectory)
import Data.Maybe(fromMaybe)

import Tasks.Task
import Tasks.Project
import Tasks.Types(bs, bsToString)
import Tasks.Cli.SimpleMenu

-- | The file path at which the currently saved task database exists (if at all)
taskFilePath :: IO FilePath
taskFilePath = liftM (</> ".taskdb") getHomeDirectory

-- | Save projects to the task file path.
-- The first project is considered to be the default project, and will be,
-- at some point, a container for tasks that aren't assigned to any particular
-- project
saveProjects :: [Project] -> IO ()
saveProjects prjs = taskFilePath >>= (`encodeFile` prjs)

-- | Read projects from the task file path
getProjects :: IO [Project]
getProjects = taskFilePath >>= decodeFile

-- | Given a task, create an array of lines, each (essentially) unformatted,
-- each line containing a corresponding task property and its respective value
taskRep :: Task -> [String]
taskRep tsk@(Task { taskName = tnm
                             , taskNotes = tns
                             , taskPriority = tp
                             , taskCompleted = tc }) =
   [ bsToString tnm
   , if bsToString tns == "" then "Notes: None" else "Notes: " ++ bsToString tns
   , "Priority: " ++ show tp
   , "Completed: " ++ (if tc then "Yes" else "No") ]

projectRep :: Project -> [String]
projectRep proj@(Project { projectName = pn
                         , projectNotes = pnts
                         , projectTasks = ptsks }) =
   [
      "Project: " ++ (bsToString pn),
      "Project Notes: " ++ (bsToString pnts)
   ] ++ (concatMap taskRep ptsks)

-- | Print a project to stdout
printProject :: Project -> IO ()
printProject proj = do
   putStr . bsToString . projectName $ proj
   putStr ": \n"
   let ftsks = map taskRep $ projectTasks proj in
      forM_ ftsks $ 
         mapM_ (putStr .
            (replicate (length spn + 1) ' ' ++) .
            (++"\n"))
   where
      spn = bsToString . projectName $ proj

-- | Read projects from the task file path and print each one
printProjects :: IO ()
printProjects = do
   projects <- getProjects
   forM_ projects $ \proj ->
      printProject proj
