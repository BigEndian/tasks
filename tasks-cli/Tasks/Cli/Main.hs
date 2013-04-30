module Tasks.Cli.Main where

import Control.Monad
import Data.Binary (decodeFile, encodeFile)
import Data.Either
import Data.List (elem)
import Data.Maybe
import System.FilePath
import System.Directory (getHomeDirectory)

import Tasks.Task
import Tasks.Project

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

-- Menus
-- | At the moment, these two custom menu constructors
listObjectsMenu :: (Rep a) => [a] -> Menu a
listObjectsMenu xs
   | length xs > 10 = error "10 objects or fewer per generic-list menu"
   | otherwise       = Menu { menuChoices = choices
                            , menuHandler = handler }
   where
      choices = map (\(n,s) -> choice $ '&' : (show n) ++ ". " ++ s) (zip [0..9] (map shortRep xs))
      handler (Choice (c:_) _, _) = return $ xs !! (read [c] :: Int)

listObjectsDirMenu :: (Rep a) => [a] -> Menu (Either Direction a)
listObjectsDirMenu xs =
      Menu { menuChoices = menuChoices ndm ++ [Choice "NnPp" "(N)ext (P)revious"] 
           , menuHandler = handler }
   where
      ndm = listObjectsMenu xs
      handler (c,k) =
         if k `elem` "NnPp" then
            return (Left $ direction k)
         else
            return (Right $ (xs !! (read [k] :: Int)))



-- Main, with knowledge of the projects
main' :: [Project] -> IO ()
main' projects = return ()
   --newprojects <- menuDisplay (listProjectsMenu projects)
   --main' newprojects

main = do
   projects <- getProjects
   main' projects
