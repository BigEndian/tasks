module Main where

import Control.Monad
import Data.Binary(decodeFile)
import System.FilePath
import System.Directory(getHomeDirectory)
import System.Console.GetOpt

import Tasks.Task
import Tasks.Project

-- | The file path at which the currently saved task database exists (if at all)
taskFilePath :: IO FilePath
taskFilePath = liftM (</> ".taskdb") getHomeDirectory

-- | A type describing the possible command line flags
data Flag = ListProjects
          | ProjectInfo Int
          | EditProject Int deriving Show

-- | The options taken by the front-facing executable when invoked
options :: [OptDescr Flag]
options = 
   [ Option "l" ["list-projects"] (NoArg ListProjects) "List all projects" ]
   --, Option "i" ["project-info"]  (ReqArg getProjectByStrNum "1") "Number of the project to view more information on" ]
   --, Option ['e'] ["project-edit"]  (ReqArg editProjectByStrNum "1") "Number of the project to edit" ]

-- | Read the projects stored in the given file
getProjects :: FilePath -> IO [Project]
getProjects = decodeFile

-- | Construct a ProjectInfo flag given a string
projectInfo :: String -> Flag
projectInfo = ProjectInfo . read

main = do
   where
      parsedOptions = getOpt Permute 
