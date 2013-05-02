-- | Module: Tasks.Cli.Menus
-- Contains all of the menu helpers and menu creators used in various
-- contexts, e.g. editing a task which is in a project or not in a project,
-- editing a project, task removal, etc.
module Tasks.Cli.Menus
   (
      Action(..)

   ,  numbered
   ,  promptAndRead

   ,  tskEditMenuHandler
   ,  tskEditMenu
   ,  tskEditPrompt
   ) where

import Control.Monad
import Data.Char (toLower)
import Data.Maybe
import Data.Either
import System.IO (hSetEcho, stdin)

import Tasks.Task
import Tasks.Project
import Tasks.Types

import Tasks.Cli.Rep
import Tasks.Cli.Menu

-- | A type representing a user's chosen direction
data Direction = DLeft | DRight deriving (Show, Read, Eq, Ord)

-- | A type meant to describe actions which may be
-- performed on Tasks and Projects. For example, editing
-- a task's name, dissociating a task,
-- from a project, deleting a task, deleting a project, etc.
data Action t = Delete t | Modified t | Unmodified t deriving (Show, Read, Eq, Ord)

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

-- | Ask the user whether they're certain they'd like to delete the object
-- described by the given string
deletionPrompt :: String -> IO Bool
deletionPrompt obs = do
   putStr $ "Are you sure you want to delete this " ++ obs ++ "? (y/n) "
   c <- liftM toLower getChar
   putStrLn ""
   if c == 'y' || c == 'n' then
      return (c == 'y')
      else
      deletionPrompt obs

-- | Choices for organization, such as task/project deletion
orgChoices :: String -> [Choice]
orgChoices obs =
      map (\(Choice chs s) -> Choice chs (s ++ obs)) choices ++ [Choice "Qq" "(Q)uit editing"]
   where
      choices = [ Choice "Dd" "(D)elete " ]

-- | Given the object itself, and a string whose value represents the object,
-- construct a handler to be used to confer that the user chose to delete/move/etc
-- an object.
orgHandler :: a -> String -> (Choice, Char) -> IO (Action a)
orgHandler obj obs (Choice chrs _, chr)
   | chr `elem` "Dd" = do
      res <- deletionPrompt obs;
      if res then
         return $ Delete obj
         else
            return $ Unmodified obj
   | chr `elem` "Qq" = return $ Unmodified obj
   | otherwise = return $ Unmodified obj

-- | The menu used to display organizational choices to the user
orgMenu :: a -> String -> Menu (Action a)
orgMenu obj obs = Menu { menuChoices  = orgChoices obs
                       , menuHandler  = orgHandler obj obs
                       , menuSubmenus = [] }

dirChoices :: [Choice]
dirChoices = [ Choice "Ll" "Go (l)eft", Choice "Rr" "Go (r)ight" ]

dirHandler :: (Choice, Char) -> IO Direction
dirHandler (Choice "Ll" _, _) = return DLeft
dirHandler (Choice "Rr" _, _) = return DRight
dirHandler _ = error "Invalid choice for a direction"

dirMenu :: Menu Direction
dirMenu = Menu { menuChoices = dirChoices
               , menuHandler = dirHandler
               , menuSubmenus = [] }

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
tskEditMenuHandler :: Task -> (Choice, Char) -> IO (Action Task)
tskEditMenuHandler tsk c@(Choice chrs _, chr)
   | chrs == "Qq" = return $ Unmodified tsk
   | chrs == "Nn" = do
      newName <- inpString
      return $ Modified tsk { taskName = newName }
   | chrs == "Oo" = do
      newNotes <- inpString
      return $ Modified $ 
         tsk { taskMetadata = tmd { mdNotes = notesOrNothing newNotes } }
   where
      tmd = taskMetadata tsk
      prompt = tskEditPrompt chr
      inpString = liftM bs $ promptAndRead prompt
      notesOrNothing bs = if bsEmpty bs then Nothing else Just bs

-- | The menu creation method used for editing a particular task.
-- Currently only allows for the editing of a tasks's notes and name
tskEditMenu :: Task -> Menu (Action Task)
tskEditMenu tsk = Menu { menuChoices   = choices
                       , menuHandler   = tskEditMenuHandler tsk
                       , menuSubmenus  = [orgMenu tsk "Task"] }
   where
      tn = bsToString (taskName tsk)
      tns = bsToString (fromMaybe (bs "None") $ taskNotes tsk)
      choices = [ Choice "Nn" ("Task (N)ame:  " ++ tn)
                , Choice "Oo" ("Task N(o)tes: " ++ tns) ]

tsksListChoices :: [Task] -> [Choice]
tsksListChoices tsks =
   let ten = take 10 tsks in
      numbered ten

tsksListHandler tsks (_, chr) =
      menuRun $ menuWithMod (return . Right) $ tskEditMenu chosen
   where
      chosen = tsks !! (read [chr])

tsksListMenu :: [Task] -> Menu (Either Direction (Action Task))
tsksListMenu tsks =
   Menu { menuChoices = tsksListChoices tsks
        , menuHandler = tsksListHandler tsks
        , menuSubmenus = [menuWithMod (return . Left) dirMenu] }
