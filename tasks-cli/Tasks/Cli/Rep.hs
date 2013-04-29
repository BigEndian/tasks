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
   --,  tskShortRep
   --,  tskLongRep
   ) where

import Data.Maybe (fromMaybe, fromJust)

import Tasks.Types
import Tasks.Task
import Tasks.Project


type ShortRep a = a -> String

type LongRep  a = a -> [String]


prjShortRep :: ShortRep Project
prjShortRep (Project { projectName = prn }) = bsToString prn

prjLongRep :: LongRep Project
prjLongRep p =
   [ prjShortRep p ++ " (" ++ (show $ projectPriority p) ++ " priority)"
   , "Notes:     " ++ pnts
   , "Completed: " ++ (show $ projectCompleted p)
   , "Due Date:  " ++ pddstr ]
   where
      pnts   = fromMaybe "None" $ fmap bsToString (projectNotes p)
      pddstr  = fromMaybe "None" $ fmap show (projectDue p)
