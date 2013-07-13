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

import Tasks.Types
import Tasks.Task
import Tasks.Project

import Tasks.Cli.Rep
import Tasks.Cli.Menu

-- | A type meant to describe actions which may be
-- performed on Tasks and Projects. For example, editing
-- a task's name, dissociating a task,
-- from a project, deleting a task, deleting a project, etc.
data Action t = Delete t | Modified t | Unmodified t
              | Quit t | ALeft | ARight deriving (Eq, Ord)

instance (Show t) => Show (Action t) where
   show act = case act of
      Modified t   -> "Modified "   ++ show t
      Unmodified t -> "Unmodified " ++ show t
      Delete t     -> "Delete "     ++ show t
      Quit t       -> "Quit "       ++ show t
      ALeft        -> "ALeft"
      ARight       -> "ARight"

isModified (Modified _) = True
isModified _            = False
isDelete (Delete _) = True
isDelete _          = False

fromAction :: (Show t) => Action t -> t
fromAction (Delete t)     = t
fromAction (Modified t)   = t
fromAction (Unmodified t) = t
fromAction (Quit t)       = t
fromAction act            = error ("Invalid action of " ++ show act)


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
orgMenu obj obs inc_org dtup = Menu { menuTitle = padString '=' 35 "Organizational Options"
                                    , menuChoices  = orgChoices obs inc_org dtup
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
tskEditMenuHandler :: Task -> Bool -> (Choice, Char) -> IO (Action Task)
tskEditMenuHandler tsk modified c@(Choice chrs _, chr)
   | chrs == "Nn" = do
      newName <- inpString
      let mtask = tsk { taskName = newName }
      menuRun $ tskEditMenu mtask True
   | chrs == "Oo" = do
      newNotes <- inpString
      let mtask = tsk { taskMetadata = tmd { mdNotes = notesOrNothing newNotes } }
      menuRun $ tskEditMenu mtask True
   where
      tmd = taskMetadata tsk
      prompt = tskEditPrompt chr
      inpString = liftM bs $ promptAndRead prompt True
      notesOrNothing bs = if bsEmpty bs then Nothing else Just bs


-- | The menu creation method used for editing a particular task.
-- Currently only allows for the editing of a tasks's notes and name.
tskEditMenu :: Task -> Bool -> Menu (Action Task)
tskEditMenu tsk mod = Menu { menuTitle = padString '=' 35 "Edit Task"
                           , menuChoices   = tskChoices tsk
                           , menuSubmenuHandler = Just wrap
                           , menuHandler   = tskEditMenuHandler tsk mod
                           , menuSubmenus  = [orgMenu tsk "Task" True (False, False)] }
   where
      wrap act@(Quit t) = return (if mod then Modified t else Unmodified t)
      -- Unmodified from orgMenu means the deletion prompt was brought up but refused
      wrap act@(Unmodified t) = menuRun $ tskEditMenu tsk mod
      wrap act = return act


-- | Currently, runs the task edit menu, determines which task is modified,
-- replaces the modified task in the task list, then runs a new task list menu
-- with the modified set of tasks.
tsksListHandler :: [Action Task] -> Int -> (Choice, Char) -> IO [Action Task]
tsksListHandler tacts bidx (chc,chr) = do
      let etidx         = bidx + (read [chr] :: Int)
      let etact         = tacts !! etidx
      let etask         = fromAction etact
      resact <- menuRun $ tskEditMenu etask False
      let modified  = isModified resact || isDelete resact
      let new_tacts = if modified then handleAction resact else tacts
      menuRun (tsksListMenu' new_tacts bidx)
   where
      (ls,_:rs) = splitAt (bidx+read [chr]) tacts
      handleAction (Delete t) = ls++rs
      handleAction act@(Modified t) = ls++[act]++rs
      handleAction (Unmodified t) = tacts
      handleAction (Quit t) = ls++[Unmodified t]++rs

tsksListSubmenuHandler :: Int -> [Action Task] -> IO [Action Task]
tsksListSubmenuHandler bidx (lact:oacts) =
   -- lact represents the action chosen on the org menu by the user
   case lact of
      Quit t       -> return oacts
      Unmodified t -> return oacts
      ALeft        -> menuRun $ tsksListMenu' oacts (bidx-10)
      ARight       -> menuRun $ tsksListMenu' oacts (bidx+10)
      otherwise    -> fail "unmatched action from the org submenu"

tsksListMenu' :: [Action Task] -> Int -> Menu [Action Task]
tsksListMenu' acts idx =
      Menu { menuTitle = padString '=' 35 "Task List"
           , menuChoices = choices
           , menuHandler = tsksListHandler acts idx
           , menuSubmenuHandler = Just (tsksListSubmenuHandler idx)
           , menuSubmenus = [menuMod orgh orgm] }
   where
      (prior,tacts')  = splitAt idx acts
      (tacts,others)  = splitAt 10 tacts'
      ftask           = fromAction $ head acts
      hl = not . null $ prior
      hr = not . null $ others
      choices = numbered . map fromAction $ tacts
      orgh act = return $ act:acts
      orgm = orgMenu ftask "" False (hl, hr)


tsksListMenu :: [Task] -> Menu [Action Task]
tsksListMenu tasks = tsksListMenu' asActs 0
   where
      asActs = map Unmodified tasks

prjChoices :: Project -> [Choice]
prjChoices p = [ Choice "Nn" ("Project (N)ame:  " ++ pn)
               , Choice "Oo" ("Project N(o)tes: " ++ pnts)
               , Choice "Ll" "(L)ist Tasks"
               , Choice "Dd" "(D)elete Project"
               , Choice "Qq" "(Q)uit editing" ]
   where
      pn = bsToString (projectName p)
      pnts = maybe "None" bsToString (projectNotes p)

prjEditPrompt :: Char -> String
prjEditPrompt c
   | c `elem` "Nn" = "Enter new project name: "
   | c `elem` "Oo" = "Enter new project notes: "
   | otherwise = error "Invalid character given to edit prompt"

prjEditHandler :: Project -> (Choice, Char) -> IO (Action Project)
prjEditHandler p (Choice chrs _, chr) = case chrs of
      "Nn" -> inpString >>= (\nname -> return p { projectName = nname }) >>=
         return . Modified
      otherwise -> return $ Unmodified p
   where
      inpString = liftM bs $ promptAndRead (prjEditPrompt chr) True

prjEditMenu :: Project -> Menu (Action Project)
prjEditMenu p =
   Menu { menuTitle = padString '=' 35 "Edit Project"
        , menuChoices = prjChoices p
        , menuHandler = prjEditHandler p
        , menuSubmenuHandler = Nothing
        , menuSubmenus = [] }
   where
      pn = bsToString (projectName p)
