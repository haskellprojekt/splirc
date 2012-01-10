module Splirc.ModuleAPI where

import Control.Monad
import Data.String.Utils
import Network
import System.IO
import System.Environment

import Splirc.Types
import Splirc.Parser



-- IMPORT MODULES, RUN SETUP METHODS

import qualified Splirc.Modules.Pong
import qualified Splirc.Modules.Echo
import qualified Splirc.Modules.SplineChannel
setups = [Splirc.Modules.Pong.setup, Splirc.Modules.Echo.setup, Splirc.Modules.SplineChannel.setup]
-- that should of course happen automatically in the future (detect all
-- modules and search setup methods, later: only the enabled ones)



commandPrefix = "!" -- TODO: take that from the config


-- HANDLE SERVER RESPONSES, THROW EVENTS

handleFromServer :: State -> FromServer -> IO ()
handleFromServer state (NickCommand nick "JOIN" [channelname]) = if nick == st_nick state
    then event state (SelfJoinEvent channelname)
    else event state (JoinEvent channelname) -- to do change the Constructor to also include a parameter for the nick of the user who has joined!
handleFromServer state (PureCommand "PING" [param]) = event state (PingEvent param)
handleFromServer state (NickCommand nick "PRIVMSG" [channel@('#':_), msg]) = do
    event state (MessageEvent channel nick msg)
    handleCommandEvent state (CommandEvent channel nick) msg
handleFromServer state (NickCommand nick "PRIVMSG" [receiver, msg]) = if receiver == st_nick state
    then do
        event state (PrivMessageEvent nick msg)
        handleCommandEvent state (PrivCommandEvent nick) msg
    else putStrLn $ "Received Private Message to someone else (namely " ++ receiver ++ ")"


-- fallbacks
handleFromServer state cmd@(PureCommand msg params) = putStrLn $ "unknown PureCommand received :" ++ show cmd
handleFromServer state cmd = putStrLn $ "unknown command received: " ++ show cmd
{-handleFromServer state (NickCommand nick "PART" [channelname]) = if nick == st_nick state
    then event $ SelfPartEvent nick channelname
    else event $ PartEvent nick channelname-}

-- RECEIVE EVENTS AND CALL EVENT HANDLERS

-- Called everytime something happens.
-- Try to match the event to every registered event handler using
-- `applyEvent`, concat the reactions, and pass it to handleReactions.
-- It is convenient to do `event = event_ h handlers` and then just `event e`
-- - handlers is the list of registered event handlers
-- - e is the event that happened
event :: State -> Event -> IO ()
--event handlers e = concat $ map (applyEvent e) handlers
event st e = handleReactions st reactions
    where
        reactions = (liftM concat . sequence) $ map (applyEvent e) handlers
        handlers = st_handlers st
-- that liftM thing is again just the "IO version" of concat


-- Called by event. Only if the EventHandler matches the Event, the
-- EventHandler is called.
-- a little ugly, too, maybe this can be done better, with some monad foo or so
applyEvent :: Event -> EventHandler -> IO [Reaction]
applyEvent e@(MessageEvent ch1 _ _) (OnMessage ch2 f) = onlyIfMatch ch1 ch2 (f e)
applyEvent e@(MessageEvent _ _ _) (OnEveryMessage f) = f e
applyEvent e@(ConnectEvent) (OnConnect f) = f e
applyEvent e@(JoinEvent _) (OnEverySelfJoin f) = f e
applyEvent e@(JoinEvent ch1) (OnSelfJoin ch2 f) = onlyIfMatch ch1 ch2 (f e)
applyEvent e@(PingEvent msg) (OnPing f) = (f e)
applyEvent e@(CommandEvent ch user cmd1 args) (OnCommand cmd2 f) = onlyIfMatch cmd1 cmd2 (f e)
applyEvent e@(PrivCommandEvent user cmd1 args) (OnPrivCommand cmd2 f) = onlyIfMatch cmd1 cmd2 (f e)
-- ...
applyEvent msg onBla = return [] -- Fallback: event does not match this EventHandler

-- helper for stuff like OnMessage and OnSelfJoin
onlyIfMatch a b result = if a == b then result else return []


-- REACTION HANDLING STUFF

handleReactions :: State -> IO [Reaction] -> IO ()
handleReactions st reactions = do
    rs <- reactions
    handleReactions' st rs

handleReactions' :: State -> [Reaction] -> IO ()
handleReactions' st [] = return ()
handleReactions' st (r:rs) = do
    handleReaction st r
    handleReactions' st rs

-- TODO: we need checks here of course, eg to check if stuff contains spaces or newlines
handleReaction :: State -> Reaction -> IO ()
handleReaction st (SendMessage ch msg) = connWrite st $ "PRIVMSG " ++ ch ++ " :" ++ msg
handleReaction st (SendCommand cmd) = handleCommand st cmd -- just to save typing
handleReaction st (Debug msg) = hPutStrLn stderr msg

handleCommand :: State -> IRCCommand -> IO ()
handleCommand st (Pong arg) = connWrite st $ "PONG :" ++ arg
handleCommand st (Join ch) = connWrite st $ "JOIN :" ++ ch
handleCommand st (RawCommand cmd) = connWrite st cmd

-- helper
connWrite :: State -> String -> IO ()
connWrite st msg = do
    putStrLn $ "> " ++ msg
    hPutStrLn h msg
    where h = st_conn st

--      handleCommandEvent (CommandEvent channel nick) nick msg
handleCommandEvent :: State -> (CommandName -> CommandArgs -> Event) -> Message -> IO ()
handleCommandEvent state base msg = when (startswith commandPrefix msg) ((putStrLn $ "command! " ++ cmd) >> (event state $ base cmd args))
    where ((_:cmd):args) = words msg
