{-# LANGUAGE DeriveDataTypeable #-}
module Tasks.Cli.Args
   (
     Args(..) -- The Args datatype.
   , defaultArgs -- A decorated instance of Args to be used by main
   ) where

import Data.Data
import Data.Typeable
import System.Console.CmdArgs

-- | The type used to represent arguments passed to this application
data Args = Args { listProjects :: Bool 
                 , editProject :: Int } deriving (Show, Data, Typeable)

mutExGroup :: String
mutExGroup = "Mutually Exclusive Flags"

-- | An instance of Args decorated with default values and help information
defaultArgs =  
   Args { listProjects = True &= help "List the current projects" &= 
            groupname mutExGroup &= name "list-projects"
        , editProject = def &= 
            help "Edit a project with the given number" &= 
            typ "INT" &= opt (0 :: Int) &= name "edit-project" } &= 
               program "tasks"
