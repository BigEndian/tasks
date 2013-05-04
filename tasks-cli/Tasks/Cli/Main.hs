module Tasks.Cli.Main where

import Control.Monad
import Data.Binary (decodeFile, encodeFile)
import System.FilePath
import System.Directory (getHomeDirectory)

import Tasks.Project

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

-- Main, with knowledge of the projects
main' :: [Project] -> IO ()
main' projects = return ()

main = do
   projects <- getProjects
   main' projects
