module Tasks.Database(Database) where


import Tasks.Task
import Tasks.TaskList(TaskList)

import System.IO
import Data.Maybe(Maybe(..))
import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Char8 as B

data Database = Database { databaseFileName :: FilePath, databaseTaskList :: IO TaskList }

{-
readTask :: Handle -> IO Task
readTask h = do
               line <- hGetLine h
               return $ task line Nothing

exHandle = openFile "/home/eric/.tasks" ReadMode -}

databaseFile :: GenParser Task st TaskList
databaseFile =
   do result <- many taskLine
      eof
      return result

taskLine :: GenParser Char st Task
taskLine =
   do name  <- taskInfoName
      return $ task name Nothing
