{-# LANGUAGE TypeSynonymInstances #-}
module Tasks.Cli.Types
   (
      SerProject (..)
   ,  serProj
   
   ,  serProjIndices
   ,  serProjProject

   , unserProj
   , serProjects
   , unserProjects

   , exSerProject

   , DBFormat(..)
   , exDBFormat
   , dbFormatSave

   , (^||^)
   , (^&&^)

   ) where


import Control.Monad
import Data.Binary
-- import qualified Data.ByteString as BW
import Data.List (elemIndex)

import System.Directory
import System.FilePath

import Tasks.Task
import Tasks.Project

-- SerProject (with project tasks removed when given) and 
-- an array of integers representing the indexes of the tasks.
-- This way you can create a table of tasks at one central location, and
-- refer to them with ints otherwise
data SerProject = SerProject Project [Int] deriving (Show, Read, Eq)

instance Binary SerProject where
   put (SerProject prj ints) = put prj >> put ints
   get = do
      prj <- get
      ints <- get
      return $ SerProject prj ints


serProj' :: [Task] -> [Task] -> [Int]
serProj' alltasks [] = []
serProj' [] ptasks = []
serProj' alltasks (pt:pts) = case pt `elemIndex` alltasks of
   (Just idx) -> idx : serProj' alltasks pts
   otherwise -> serProj' alltasks pts

-- serProject takes a project and a set of tasks, looking up the project's
-- tasks and building a list of ints representing the project's tasks respective
-- indexes in the passed task list
serProj :: Project -> [Task] -> SerProject
serProj prj [] = SerProject prj []
serProj prj@(Project { projectTasks = ptsks }) alltasks
   | null ptsks = SerProject prjtskless []
   | otherwise = SerProject prjtskless (serProj' alltasks ptsks)
   where
      prjtskless = prj { projectTasks = [] }

-- unserProj takes a serialized project and a set of coresponding tasks and
-- constructs the original project using it.
unserProj :: SerProject -> [Task] -> Project
unserProj (SerProject prj sints) alltasks =
   prj { projectTasks = map (alltasks!!) sints }

-- serProjProject takes a serialized project and returns its contained project
serProjProject :: SerProject -> Project
serProjProject (SerProject prj _) = prj

-- serProjIndices takes a serialized project and returns its indexes
serProjIndices :: SerProject -> [Int]
serProjIndices (SerProject _ ints) = ints

serProjects :: [Project] -> [Task] -> [SerProject]
serProjects [] _ = []
serProjects _ [] = []
serProjects prjs tsks = map (flip serProj $ tsks) prjs

unserProjects :: [SerProject] -> [Task] -> [Project]
unserProjects [] _ = []
unserProjects _ [] = []
unserProjects sprjs tsks = map (flip unserProj $ tsks) sprjs

exSerProject = serProj exProject1 exTasks
encSerProject = encode exSerProject
decSerProject = (decode encSerProject) :: SerProject

-- Tasks at the beginning of the file, projects afterwards
data DBFormat = DBFormat [Task] [Project] deriving (Show, Read)

instance Binary DBFormat where
   get = do
      tasks <- get
      serProjs <- get
      return $ DBFormat tasks (unserProjects serProjs tasks)

   put (DBFormat tasks projs) = do
      put tasks
      put (serProjects projs tasks)

exDBFormat = DBFormat exTasks exProjects
encDBFormat = encode exDBFormat
decDBFormat = (decode encDBFormat) :: DBFormat

-- Monadic boolean or, written to prevent unnecessary binding
(^||^) :: (Monad m) => m Bool -> m Bool -> m Bool
l ^||^ r = l >>= \lval ->
   if lval == True then
      return lval
      else
         r

-- Monadic boolean and, written to prevent unnecessary binding
(^&&^) :: (Monad m) => m Bool -> m Bool -> m Bool
l ^&&^ r = l >>= \lval ->
   if lval == False then
      return False
      else
         r

{-(^|||^) :: (Monad m) => (a -> Bool) -> m a -> (m a -> m a)
f ^|||^ m1 = -}

dbFormatSave :: DBFormat -> FilePath -> IO Bool
dbFormatSave (DBFormat tasks project) fp = do
      fp <- fixedfp
      dfe fp ^||^ validPerms perm
   where
      dfe fp = putStrLn "dfe" >> doesFileExist fp
      fixedfp = canonicalizePath $ normalise fp -- normalize the path
      perm = getPermissions fp
      validPerms p = (liftM and) $ mapM (\f -> f p) (map liftM [readable, writable])
