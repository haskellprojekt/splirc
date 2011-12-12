module Splirc.Modules.SplineChannel where

import Splirc.Types

channelname = "#spline"

setup :: IO [EventHandler]
setup = return [OnConnect connectHandler, OnJoin channelname sayHelloToSpline]

connectHandler _ = do
    putStrLn "connectHandler called"
    return [SendCommand $ Join channelname]
sayHelloToSpline (IsJoin _) = return
    [SendMessage channelname "hello world^Wspline!"]
