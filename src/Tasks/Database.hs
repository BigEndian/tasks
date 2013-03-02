module Tasks.Database(Database) where


import Tasks.Task
import Tasks.TaskList

import System.IO
import Data.Maybe(Maybe(..))
import qualified Data.ByteString as B
import Data.Word(Word8(..))
import Data.Binary

data Database = Database { databaseFileName :: FilePath, databaseTaskList :: IO TaskList }

exHandle = openFile "/home/eric/.tasks" ReadMode

