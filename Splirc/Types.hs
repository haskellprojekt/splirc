module Splirc.Types where

import GHC.IO.Handle.Types

data EventHandler = OnMessage Channel EventHandlerFunction
                  | OnEveryMessage EventHandlerFunction
                  | OnPrivMessage EventHandlerFunction
                  | OnEveryResponse EventHandlerFunction
                  | OnConnect EventHandlerFunction
                  | OnEveryJoin EventHandlerFunction
                  | OnJoin Channel EventHandlerFunction
                  | OnCommand CommandName EventHandlerFunction
type EventHandlerFunction = Event -> IO [Reaction]

data Event = IsMessage Channel User
           | IsPrivMessage User
           | IsResponse String
           | IsConnect
           | IsJoin Channel
           | IsCommand CommandName [String]

type Channel = String
type User = String
type Message = String
type CommandName = String -- a user command
data Reaction = SendMessage Channel Message
              | SendCommand IRCCommand
              | Debug String
data IRCCommand = RawCommand String -- an IRC command sent to the server
                | Pong String
                | Join Channel

type Connection = GHC.IO.Handle.Types.Handle

data State = State {
    st_conn :: Connection,
    st_handlers :: [EventHandler]
}
