module Tasks.Cli.Main where

import Control.Monad
import Data.Binary (decodeFile, encodeFile)
import Data.Either
import Data.List (elem)
import Data.Maybe
import System.FilePath
import System.Directory (getHomeDirectory)
import System.IO (hSetEcho, stdin)

import Tasks.Task
import Tasks.Project
import Tasks.Types

import Tasks.Cli.Rep
import Tasks.Cli.Menu

data Direction = Next | Previous deriving (Eq, Ord, Show, Read)

direction :: Char -> Direction
direction c
   | c `elem` "Nn" = Next
   | c `elem` "Pp" = Previous
   | otherwise     = error "Invalid character passed to direction"

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

-- | Given a list of representable objects,
-- return a set of choices representing those
-- objects.
-- The list must not contain more than 10 elements, and the resultant
-- choices are numbered 0 through 9, both in output and in
-- which key is used to select them.
numbered :: (Rep a) => [a] -> [Choice]
numbered xs
   | null xs   = []
   | l <= 10   = zipWith nchc [0..9] xs
   | otherwise = error "Given list must not have more than 10 elements"
   where
      l = length xs
      nchc n x = let sn = show n in
         Choice sn  (sn ++ ". " ++ shortRep x)

-- | Present a prompt to the user and get a string back from them
promptAndRead :: String -> IO String
promptAndRead prompt =
   putStr prompt >> hSetEcho stdin True >> getLine >>= 
      (\ln -> hSetEcho stdin False >> return ln)

-- | Determine the prompt to be displayed given a certain
-- character inputted by the user
taskEditPrompt :: Char -> String
taskEditPrompt chr
   | chr `elem` "Nn" = "Enter new task name: "
   | chr `elem` "Oo" = "Enter new task notes: "
   | otherwise       = error "Invalid character"

-- | The menu handler for the edit task menu.
-- Returns Nothing if no changes were made, otherwise
-- returns the modified task
taskEditMenuHandler :: Task -> (Choice, Char) -> IO (Maybe Task)
taskEditMenuHandler tsk (Choice chrs _, chr)
   | chrs == "Qq" = return Nothing
   | chrs == "Nn" = do
      newName <- inpString
      return $ Just $ tsk { taskName = newName }
   | chrs == "Oo" = do
      newNotes <- inpString
      return $ Just $ 
         tsk { taskMetadata = tmd { mdNotes = notesOrNothing newNotes } }
   where
      tmd = taskMetadata tsk
      prompt = taskEditPrompt chr
      inpString = liftM bs $ promptAndRead prompt
      notesOrNothing bs = if bsEmpty bs then Nothing else Just bs

-- | The menu creation method used for editing a particular task.
-- Currently only allows for the editing of a tasks's notes and name
taskEditMenu :: Task -> Menu Task (Maybe Task)
taskEditMenu tsk = Menu { menuChoices  = choices
                        , menuInternal = tsk
                        , menuHandler = taskEditMenuHandler }
   where
      tn = bsToString (taskName tsk)
      tns = bsToString (fromMaybe (bs "None") $ taskNotes tsk)
      choices = [ Choice "Nn" ("Task (N)ame:  " ++ tn)
                , Choice "Oo" ("Task N(o)tes: " ++ tns)
                , Choice "Qq" "(Q)uit editing" ]

{-
projectEditMenu :: Project -> Menu Project (Maybe Project)
projectEditMenu prj = Menu { menuChoices = choices
                           , menuInternal = prj
                           , menuHandler = projectEditMenuHandler }
   where
      pn      = bsToString (projectName prj)
      ptsks   = projectTasks prj
      choices = -}


-- Main, with knowledge of the projects
main' :: [Project] -> IO ()
main' projects = return ()
   --newprojects <- menuDisplay (listProjectsMenu projects)
   --main' newprojects

main = do
   projects <- getProjects
   main' projects
