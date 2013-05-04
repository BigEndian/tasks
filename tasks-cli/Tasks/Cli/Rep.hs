-- |
-- Module: Tasks.Cli.Rep
--
-- Contains the Rep typeclass, used for types
-- which can be given short and long representations,
-- useful for Menus
module Tasks.Cli.Rep
   (
      Rep(..)
   ) where

import Data.Maybe (maybe)

import Tasks.Types
import Tasks.Task
import Tasks.Project

class Rep a where
   shortRep :: a -> String
   longRep  :: a -> [String]
   typeName :: String


instance Rep Project where
   typeName = "Project"
   shortRep = bsToString . projectName
   longRep p =
      [ shortRep p ++ " (" ++ show (projectPriority p) ++ " priority)"
      , "Notes:     " ++ pnts
      , "Completed: " ++ show (projectCompleted p)
      , "Due Date:  " ++ pddstr ]
      where
         pnts   = maybe "None" bsToString (projectNotes p)
         pddstr  = maybe "None" show (projectDue p)

instance Rep Task where
   typeName = "Task"
   shortRep = bsToString . taskName
   longRep t = 
      [ shortRep t ++ " (" ++ show (taskPriority t) ++ " priority)"
      , "Notes:     " ++ tnts
      , "Completed: " ++ show (taskCompleted t)
      , "Due Date:  " ++ tddstr ]
      where
         tnts   = maybe "None" bsToString (taskNotes t)
         tddstr = maybe "None" show (taskDue t)
