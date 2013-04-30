-- | Module: Tasks.Cli.Menus
-- Contains all of the menu helpers and menu creators used in various
-- contexts, e.g. editing a task which is in a project or not in a project,
-- editing a project, task removal, etc.
module Tasks.Cli.Menus
   (
      numbered
   ,  promptAndRead

   ,  tskEditMenuHandler
   ,  tskEditMenu
   ,  tskEditPrompt
   ) where

import Control.Monad
import Data.Maybe
import System.IO (hSetEcho, stdin)

import Tasks.Task
import Tasks.Project
import Tasks.Types

import Tasks.Cli.Rep
import Tasks.Cli.Menu

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
tskEditPrompt :: Char -> String
tskEditPrompt chr
   | chr `elem` "Nn" = "Enter new task name: "
   | chr `elem` "Oo" = "Enter new task notes: "
   | otherwise       = error "Invalid character"

-- | The menu handler for the edit task menu.
-- Returns Nothing if no changes were made, otherwise
-- returns the modified task
tskEditMenuHandler :: Task -> (Choice, Char) -> IO (Maybe Task)
tskEditMenuHandler tsk (Choice chrs _, chr)
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
      prompt = tskEditPrompt chr
      inpString = liftM bs $ promptAndRead prompt
      notesOrNothing bs = if bsEmpty bs then Nothing else Just bs

-- | The menu creation method used for editing a particular task.
-- Currently only allows for the editing of a tasks's notes and name
tskEditMenu :: Task -> Menu Task (Maybe Task)
tskEditMenu tsk = Menu { menuChoices  = choices
                        , menuInternal = tsk
                        , menuHandler = tskEditMenuHandler }
   where
      tn = bsToString (taskName tsk)
      tns = bsToString (fromMaybe (bs "None") $ taskNotes tsk)
      choices = [ Choice "Nn" ("Task (N)ame:  " ++ tn)
                , Choice "Oo" ("Task N(o)tes: " ++ tns)
                , Choice "Qq" "(Q)uit editing" ]
