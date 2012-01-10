module Splirc.Modules.SplineChannel where

import Splirc.Types

channelname = "#splirc"

setup :: IO [EventHandler]
setup = return [OnConnect connectHandler, OnSelfJoin channelname sayHelloToSpline, OnEverySelfJoin sayHello]

connectHandler _ = return [SendCommand $ Join channelname]

sayHelloToSpline (SelfJoinEvent _) = return
    [SendMessage channelname "hello world^Wspline!"]

sayHello (SelfJoinEvent ch) = return [SendMessage ch ("hallo "++ ch)]

