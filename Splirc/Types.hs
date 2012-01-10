module Splirc.Types where

import GHC.IO.Handle.Types

-- These are the events Modules can register for.
-- Every time something matching happens, the given function is called with
-- the Event object as parameter.
-- Multiple Handlers can be set, all matching ones are called
data EventHandler = OnMessage Channel EventHandlerFunction
                  | OnEveryMessage EventHandlerFunction
                  | OnPrivMessage EventHandlerFunction
                  | OnEveryResponse EventHandlerFunction
                  | OnPing EventHandlerFunction
                  | OnConnect EventHandlerFunction
                  | OnEverySelfJoin EventHandlerFunction -- we joined
                  | OnSelfJoin Channel EventHandlerFunction -- "
                  | OnJoin Channel EventHandlerFunction -- a user joined
                  | OnEveryJoin EventHandlerFunction -- "
                  | OnCommand CommandName EventHandlerFunction
                  | OnPrivCommand CommandName EventHandlerFunction
type EventHandlerFunction = Event -> IO [Reaction] -- just a shortcut

-- These are the events that can happen. They are given to the event handler
-- so that it can find out what happened.
data Event = MessageEvent Channel User Message
           | PrivMessageEvent User Message
           | ResponseEvent FromServer
           | PingEvent String
           | ConnectEvent
           | JoinEvent Channel
           | SelfJoinEvent Channel
           | CommandEvent Channel User CommandName [String]
           | PrivCommandEvent User CommandName [String]
  deriving(Show)

type Channel = String
type User = String
type Message = String
type CommandName = String -- this is a user command (!dosomething)
type CommandArgs = [String]
data Reaction = SendMessage Channel Message
              | SendCommand IRCCommand
              | Debug String
data IRCCommand = RawCommand String -- an IRC command sent to the server
                | Pong String
                | Join Channel

type Connection = GHC.IO.Handle.Types.Handle
-- A state object which holds objects we may need, so we can pass them around
data State = State {
    st_nick :: String,
    st_conn :: Connection,
    st_handlers :: [EventHandler]
}


data FromServer = ServerCommand ServerName Command Params | NickCommand NickName Command Params | PureCommand Command Params | Unknown [String] deriving(Show)
type ServerName = String
type NickName = String
type Command = String
type Params = [String]
