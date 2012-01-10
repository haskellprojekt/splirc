module Splirc.Modules.Echo where

import Splirc.Types

setup :: IO [EventHandler]
setup = return [OnCommand "echo" echoHandler, OnPrivCommand "echo" privEchoHandler]

echoHandler :: Event -> IO [Reaction]
echoHandler (CommandEvent channel user "echo" args) =
    return [SendMessage channel $ user ++ ": " ++ unwords args]

privEchoHandler :: Event -> IO [Reaction]
privEchoHandler (PrivCommandEvent user "echo" args) =
    return [SendMessage user $ unwords args]

