-- | Module: Tasks.Cli.Menus
-- Contains all of the menu helpers and menu creators used in various
-- contexts, e.g. editing a task which is in a project or not in a project,
-- editing a project, task removal, etc.
module Tasks.Cli.Menus
   (
      Action(..)

   ,  numbered
   ,  promptAndRead

   ,  tskEditMenu
   ,  tsksListMenu
   ) where

import Control.Monad
import Data.Char (toLower)
import Data.Maybe
import System.IO (hSetEcho, stdin)
import System.Console.Readline (readline)

import Tasks.Task
import Tasks.Types

import Tasks.Cli.Rep
import Tasks.Cli.Menu

-- | A type meant to describe actions which may be
-- performed on Tasks and Projects. For example, editing
-- a task's name, dissociating a task,
-- from a project, deleting a task, deleting a project, etc.
data Action t = Delete t | Modified t | Unmodified t
              | Quit t | ALeft | ARight deriving (Show, Read, Eq, Ord)

isDelete (Delete _) = True
isDelete _ = False
isQuit (Quit _) = True
isQuit _ = False
fromAct (Delete t) = t
fromAct (Modified t) = t
fromAct (Quit t) = t
fromAct _ = error "Can't retrive a value from a type constructor which doesn't take one!"

modifyAt :: Int -> [a] -> a -> [a]
modifyAt idx xs nx = let (ls,_:rs) = splitAt idx xs in
   ls ++ [nx] ++ rs

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

-- | Present a prompt to the user and get a string back from them.
promptAndRead :: String -> IO String
promptAndRead prompt =
      hSetEcho stdin True >> keepReading >>= 
         (\ln -> hSetEcho stdin False >> return ln)
   where
      keepReading = readline prompt >>=
         maybe keepReading return

-- | Ask the user whether they're certain they'd like to delete the object
-- described by the given string.
deletionPrompt :: String -> IO Bool
deletionPrompt obs = do
   putStr $ "Are you sure you want to delete this " ++ obs ++ "? (y/n) "
   c <- liftM toLower getChar
   putStrLn ""
   if c == 'y' || c == 'n' then
      return (c == 'y')
      else
      deletionPrompt obs

-- | Choices for organization, such as task/project deletion, as well as
-- directions.
--
-- The first option is a name to represent the object, e.g. "task".
-- The second option indicates whether or not to include organizational
-- choices (e.g. delete, move, disassociate from project).
-- The final option is a tuple of booleans representing whether to include
-- the left and right directional choices, respectively.
orgChoices :: String -> Bool -> (Bool,Bool) -> [Choice]
orgChoices obs inc_org (hl,hr) =
      map (\(Choice chs s) -> Choice chs (s ++ obs)) choices ++ [Choice "Qq" "(Q)uit editing"]
   where
      org_choices = [Choice "Dd" "(D)elete " | inc_org]
      cl = Choice "Ll" "Go (l)eft"
      cr = Choice "Rr" "Go (r)ight"
      dir_choices = [cl | hl] ++ [cr | hr]
      choices = org_choices ++ dir_choices

-- | Given the object itself, and a string whose value represents the object,
-- construct a handler to be used to confer that the user chose to delete/move/etc
-- an object.
orgHandler :: a -> String -> (Choice, Char) -> IO (Action a)
orgHandler obj obs (Choice chrs _, chr)
   | chr `elem` "Dd" = do
      res <- deletionPrompt obs;
      return (if res then Delete obj else Unmodified obj)
   | chr `elem` "Qq" = return $ Quit obj
   | chr `elem` "Rr" = return ARight
   | chr `elem` "Ll" = return ALeft

-- | The menu used to display organizational choices to the user.
orgMenu :: a -> String -> Bool -> (Bool,Bool) -> Menu (Action a)
orgMenu obj obs inc_org dtup = Menu { menuChoices  = orgChoices obs inc_org dtup
                                    , menuHandler  = orgHandler obj obs
                                    , menuSubmenuHandler = Nothing
                                    , menuSubmenus = [] }

tskChoices :: Task -> [Choice]
tskChoices tsk = [ Choice "Nn" ("Task (N)ame:  " ++ tn)
                 , Choice "Oo" ("Task N(o)tes: " ++ tns) ]
   where
      tn = bsToString (taskName tsk)
      tns = bsToString (fromMaybe (bs "None") $ taskNotes tsk)

-- | Determine the prompt to be displayed given a certain
-- character inputted by the user.
tskEditPrompt :: Char -> String
tskEditPrompt chr
   | chr `elem` "Nn" = "Enter new task name: "
   | chr `elem` "Oo" = "Enter new task notes: "
   | otherwise       = error "Invalid character"

-- | The menu handler for the edit task menu.
-- Returns Nothing if no changes were made, otherwise
-- returns the modified task.
tskEditMenuHandler :: Task -> (Choice, Char) -> IO (Action Task)
tskEditMenuHandler tsk c@(Choice chrs _, chr)
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
-- Currently only allows for the editing of a tasks's notes and name.
tskEditMenu :: Task -> Menu (Action Task)
tskEditMenu tsk = Menu { menuChoices   = tskChoices tsk
                       , menuSubmenuHandler = Nothing
                       , menuHandler   = tskEditMenuHandler tsk
                       , menuSubmenus  = [orgMenu tsk "Task" True (False, False)] }

tsksListHandler :: [Task] -> Int -> (Choice, Char) -> IO (Int, Action Task)
tsksListHandler tsks idx (chc,chr) = do
      let etask = tsks !! (read [chr])
      let tem t = menuMod (\a -> return (idx, a)) (tskEditMenu t)
      (_,act) <- menuRunModifying etask
         (\t (_,act) -> (t, tem (fromAct act)))
         stopAt
         (tem etask)
      let new_tasks = handleAction (idx,act)
      menuRun (tsksListMenu new_tasks)
   where
      (ls,_:rs) = splitAt idx tsks
      stopAt (_,act) = isQuit act || isDelete act
      handleAction (idx, Delete t) = ls++rs
      handleAction (idx, Modified t) = modifyAt idx tsks t
      handleAction (idx, Unmodified t) = tsks
      -- If a quit was passed, it doesn't mean that no changes were made. This will
      -- (eventually) be fixed
      handleAction (idx, Quit t) = modifyAt idx tsks t

tsksListSubmenuHandler :: [Task] -> Int -> (Int, Action Task) -> IO (Int, Action Task)
tsksListSubmenuHandler tsks i = smh
   where
      smh tup@(idx, ARight) = menuRun $ tsksListMenu' tsks (i+10)
      smh tup@(idx, ALeft)  = menuRun $ tsksListMenu' tsks (i-10)
      smh tup = return tup

tsksListMenu' :: [Task] -> Int -> Menu (Int, Action Task)
tsksListMenu' tsks idx = Menu { menuChoices = choices
                             , menuSubmenuHandler = Just (tsksListSubmenuHandler tsks idx)
                             , menuHandler = tsksListHandler tsks idx
                             , menuSubmenus = [menuMod (\a -> return (idx, a)) $ orgMenu exTask1 "" False (hl, hr) ]}
   where
      (prior,tasks')  = splitAt idx tsks
      (tasks,others)  = splitAt 10 tasks'
      hl = not . null $ prior
      hr = not . null $ others
      choices = numbered tasks

tsksListMenu :: [Task] -> Menu (Int, Action Task)
tsksListMenu tasks = tsksListMenu' tasks 0
