module Main where

import Control.Monad
import System.FilePath
import System.Directory(getHomeDirectory)
import Data.Binary(encodeFile, decodeFile)
import Data.Maybe

import Tasks.Task

-- | The file path at which the currently saved task resides (if at all).
taskFilePath :: IO FilePath
taskFilePath = liftM (</> ".task") getHomeDirectory

-- Choice related functions and values.
data Choice = ReadFromFile
            | WriteToFile
            | Exit deriving (Eq, Show, Read, Enum, Bounded)

-- | Convert a choice to a string suitable for showing
-- the user
choiceToString :: Choice -> String
choiceToString c = case c of
   ReadFromFile -> "Read from a file"
   WriteToFile  -> "Write to a file"
   Exit         -> "Exit the program"

-- | A list of all possible choices.
choices :: [Choice]
choices = [ReadFromFile .. Exit]

-- | Prompt the user for various information used to create a task.
promptTask :: IO Task
promptTask = do
      [tn,tns,tp] <- mapM promptAndGet
         [ "Task Name"
         , "Task Notes (if any)"
         , "Task Priority (from 0 upwards)" ]
      return (task tn (Just tns) (Just (read tp :: Int)))
   where
      promptAndGet :: String -> IO String
      promptAndGet s = putStr (s ++ ": ") >> getLine

-- | Prompt the user on what to do, either save a new task or show the old one.
promptChoices :: IO Choice
promptChoices = do
      forM_ cps (\(n,c) ->
         putStrLn $ show n ++ ". " ++ choiceToString c)
      scn <- getLine;
      return $ getChoice (read scn :: Int)
   where
      getChoice num =
         let idx = num - 1 in choices !! idx
      cl = length choices :: Int
      cns = [1..cl]
      cps = zip cns choices

readTaskFromFile :: FilePath -> IO Task
readTaskFromFile = decodeFile

writeTaskToFile :: FilePath -> Task -> IO ()
writeTaskToFile = encodeFile

-- | Handle a choice from the user, returning a corresponding task if
-- they chose to make a new one, or read the old one from the task file path.
handleChoice :: Choice -> IO (Maybe Task)
handleChoice c = do
   fp <- taskFilePath;
   case c of
      ReadFromFile -> return . Just =<< readTaskFromFile fp
      WriteToFile -> 
         promptTask >>= (\t -> writeTaskToFile fp t >> return (Just t))
      _ -> return Nothing

main :: IO ()
main = do
   mapM_ putStrLn ["What would you like to do?", ""]
   c <- promptChoices
   putStrLn $ "You chose " ++ choiceToString c
   mt <- handleChoice c
   case c of
      ReadFromFile -> putStrLn ("Task is " ++ show (fromJust mt)) >> main
      WriteToFile  -> putStrLn ("Wrote the task " ++ show (fromJust mt)) >> main
      Exit -> return ()
