{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Tasks.Cli.Cli(
   --
   ) where

import Data.Binary
import qualified Data.ByteString as B
import Data.Maybe

import System.Console.CmdArgs


import Tasks.Project
import Tasks.Types
import Tasks.Cli.Types

(>>>) :: a -> (a -> b) -> b
val >>> f = f val

dbPath :: FilePath
dbPath = "/home/eric/.tasks.db"

loadDB :: FilePath -> IO DBFormat
loadDB = decodeFile

--loadDB pth = readFile pth >>= (return . decode . bs)

projectStr :: Project -> Int -> String
projectStr prj idx = 
   "Project " ++ (show idx) ++ ": " ++ (bsToString $ projectName prj) ++ " " ++
   "(" ++ (show . length . projectTasks $ prj) ++ " tasks, " ++
   (show . projectPriority $ prj) ++ " priority)"

data Opts = Opts { listProjects :: Bool
                 , editProject  :: Int
                 , newProject   :: Bool } deriving (Data, Typeable, Show, Eq)


myOpts = Opts { listProjects = True &= name "l" &= help "List all projects"
              , editProject  = -1   &= name "e" &= help "Edit a project, given its index"
              , newProject = False  &= name "n" &= help "Create a project" }


type Choice = (String, String)
choiceStr :: Choice -> String
choiceStr (keys, message) = "[" ++ keys ++ "] - " ++ message


-- Format is message, keys
editProjectChoices :: Project -> [(String, String)]
editProjectChoices prj@Project{..} = 
      [("n", "Project Name:  " ++ bsToString projectName),
       ("N", "Project Notes: " ++ noteLine ++ "...")]
   where
      newl = convertCharToWord8 '\n'
      noteLines = B.split newl (fromMaybe (bs "") (projectNotes prj))
      noteLine = if length noteLines >= 1 then
                    bsToString $ head noteLines 
                 else 
                    "No notes"

progEditProject :: Int -> IO ([Project])
progEditProject idx = do
   (DBFormat tasks projects) <- loadDB path
   return projects
         

progListProjects :: IO ()
progListProjects = do
   (DBFormat tasks projects) <- loadDB dbPath
   mapM_ (\(prj, idx) -> putStrLn $ projectStr prj idx) (zip projects [1..])

handleOpts :: Opts -> IO ()
handleOpts opts
   | listProjects opts == True = progListProjects
   | otherwise                = putStrLn "Unhandled option"

main = cmdArgs myOpts >>= handleOpts
