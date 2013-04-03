{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad
import Data.Binary(decodeFile, encodeFile)
import System.FilePath
import System.Directory(getHomeDirectory)
import Data.Maybe(fromMaybe)

import Data.Data
import Data.Typeable
import System.Console.CmdArgs

import Tasks.Task
import Tasks.Project
import Tasks.Types(tsToString, tsString)

-- | The file path at which the currently saved task database exists (if at all)
taskFilePath :: IO FilePath
taskFilePath = liftM (</> ".taskdb") getHomeDirectory

data Args = Args { listProjects :: Bool 
                 , editProject :: Int } deriving (Show, Data, Typeable)

mutExGroup :: String
mutExGroup = "Mutually Exclusive Flags"

defaultArgs =  
   Args { listProjects = True &= help "List the current projects" &= 
            groupname mutExGroup &= name "list-projects"
        , editProject = def &= 
            help "Edit a project with the given number" &= 
            typ "INT" &= opt (0 :: Int) &= name "edit-project" } &= 
               program "tasks"

saveProjects :: [Project] -> IO ()
saveProjects prjs = taskFilePath >>= (`encodeFile` prjs)

getProjects :: IO [Project]
getProjects = taskFilePath >>= decodeFile

taskRepresentation :: Task -> [String]
taskRepresentation tsk@(Task { taskName = tnm
                             , taskNotes = tns
                             , taskPriority = tp
                             , taskCompleted = tc }) =
   [ tsToString tnm
   , if tsToString tns == "" then "Notes: None" else "Notes" ++ tsToString tns
   , "Priority: " ++ show tp
   , "Completed: " ++ (if tc then "Yes" else "No") ]

printProject :: Project -> IO ()
printProject proj = do
   putStr . tsToString . projectName $ proj
   putStr ": "
   putStr "\n"
   let ftsks = map taskRepresentation $ projectTasks proj in
      forM_ ftsks $ 
         mapM_ (putStr .
            (replicate (length spn + 1) ' ' ++) .
            (++"\n"))
   where
      spn = tsToString . projectName $ proj

printProjects :: IO ()
printProjects = do
   projects <- getProjects
   forM_ projects $ \proj ->
      printProject proj

handleArgs :: Args -> IO ()
handleArgs args
   | listProjects args = printProjects
   | otherwise = return ()

main = do
   args <- cmdArgsRun $ cmdArgsMode defaultArgs
   handleArgs args
