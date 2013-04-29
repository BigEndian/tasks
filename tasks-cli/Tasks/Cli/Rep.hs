-- |
-- Module: Tasks.Cli.Rep
--
-- Contains two types, ShortRep and LongRep, each functions taking a type a and
-- returning a string or an array of strings, respectively. These types allow for more flexible display of the
-- Project and Task type
module Tasks.Cli.Rep
   (
      ShortRep
   ,  LongRep
   
   ,  prjShortRep
   ,  prjLongRep
   ,  tskShortRep
   ,  tskLongRep
   ) where

import Data.Maybe (maybe)

import Tasks.Types
import Tasks.Task
import Tasks.Project


type ShortRep a = a -> String

type LongRep  a = a -> [String]

prjShortRep :: ShortRep Project
prjShortRep = bsToString . projectName

prjLongRep :: LongRep Project
prjLongRep p =
   [ prjShortRep p ++ " (" ++ show (projectPriority p) ++ " priority)"
   , "Notes:     " ++ pnts
   , "Completed: " ++ show (projectCompleted p)
   , "Due Date:  " ++ pddstr ]
   where
      pnts   = maybe "None" bsToString (projectNotes p)
      pddstr  = maybe "None" show (projectDue p)

tskShortRep :: ShortRep Task
tskShortRep = bsToString . taskName

tskLongRep :: LongRep Task
tskLongRep t =
   [ tskShortRep t ++ " (" ++ show (taskPriority t) ++ " priority)"
   , "Notes:     " ++ tnts
   , "Completed: " ++ show (taskCompleted t)
   , "Due Date:  " ++ tddstr ]
   where
      tnts   = maybe "None" bsToString (taskNotes t)
      tddstr = maybe "None" show (taskDue t)
