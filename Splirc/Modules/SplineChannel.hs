module Splirc.Modules.SplineChannel where

import Splirc.Types

channelname = "#splirc"

setup :: IO [EventHandler]
setup = return [OnConnect connectHandler, OnSelfJoin channelname sayHelloToSpline, OnEverySelfJoin sayHello]

connectHandler _ = return [SendCommand $ Join channelname]

sayHelloToSpline (IsJoin _) = return
    [SendMessage channelname "hello world^Wspline!"]

sayHello (IsJoin ch) = return [SendMessage ch ("hallo "++ ch)]

