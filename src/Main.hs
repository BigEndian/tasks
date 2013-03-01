module Main where

import Data.Maybe

import Tasks.Task(Task, task)
import Tasks.TaskList


exampleTask = task "Do the Dishes" (Just $ "Must be done by 17:00")
main = do putStrLn "I'm a main function!"
