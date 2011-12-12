-- all together a little ugly, we should clean that up, split it in modules etc


import Control.Monad
import Network
import System.IO
import System.Environment

import Splirc.Types
import qualified Splirc.Modules.Pong
import qualified Splirc.Modules.Echo
import qualified Splirc.Modules.SplineChannel
setups = [Splirc.Modules.Pong.setup, Splirc.Modules.Echo.setup, Splirc.Modules.SplineChannel.setup]
-- that should of course happen automatically in the future (detect all
-- modules and search setup methods, later: only the enabled ones)

port = PortNumber 6667

main = do
    handlers <- runSetup
    putStrLn $ "Have " ++ show (length handlers) ++ " handlers"

    putStrLn $ "Connecting ..."
    -- we don't hardcode that to ease testing
    host:nick:args <- getArgs
    h <- connectTo host port
    hSetBuffering h NoBuffering
    hPutStrLn h $ "user " ++ nick ++ " _ _ :Splirc Haskell Bot"
    hPutStrLn h $ "nick " ++ nick

--    -- save typing
--    event <- return $ event_ h handlers

--    event IsConnect
    event_ h handlers IsConnect
    
    -- just print everything for now
    t <- hGetContents h
    putStr t

-- run the setup methods of all modules, return all event handlers.
runSetup :: IO [EventHandler] -- IO because setup methods may be IO.
runSetup = (liftM concat . sequence) setups
-- this is basically concat, but with [IO [a]] -> IO [a] instead of [[a]]->[a]


-- EVENT HANDLING STUFF

-- Called everytime something happens.
-- Try to match the event to every registered event handler using
-- `applyEvent`, concat the reactions, and pass it to handleReactions.
-- It is convenient to do `event = event_ h handlers` and then just `event e`
-- - handlers is the list of registered event handlers
-- - e is the event that happened
event_ :: Connection -> [EventHandler] -> Event -> IO ()
--event handlers e = concat $ map (applyEvent e) handlers
event_ h handlers e = handleReactions h reactions
    where reactions = (liftM concat . sequence) $ map (applyEvent e) handlers
-- that liftM thing is again just the "IO version" of concat


-- Called by event_. Only if the EventHandler matches the Event, the
-- EventHandler is called.
-- a little ugly, too, maybe this can be done better, with some monad foo or so
applyEvent :: Event -> EventHandler -> IO [Reaction]
applyEvent e@(IsMessage ch1 _) (OnMessage ch2 f) = onlyIfMatch ch1 ch2 (f e)
applyEvent e@(IsMessage _ _) (OnEveryMessage f) = f e
applyEvent e@(IsConnect) (OnConnect f) = do f e
applyEvent e@(IsJoin _) (OnEveryJoin f) = f e
applyEvent e@(IsJoin ch1) (OnJoin ch2 f) = onlyIfMatch ch1 ch2 (f e)
-- ...
applyEvent _ _ = return [] -- Fallback: event does not match this EventHandler

-- helper for stuff like OnMessage and OnJoin
onlyIfMatch a b result = if a == b then result else return []


-- REACTION HANDLING STUFF

handleReactions :: Connection -> IO [Reaction] -> IO ()
handleReactions h reactions = do
    rs <- reactions
    handleReactions' h rs

handleReactions' :: Connection -> [Reaction] -> IO ()
handleReactions' h [r] = handleReaction h r
handleReactions' h (r:rs) = do
    handleReaction h r
    handleReactions' h rs

-- TODO: we need checks here of course, eg to check if stuff contains spaces or newlines
handleReaction :: Connection -> Reaction -> IO ()
handleReaction h (SendMessage ch msg) = hPutStrLn h $ "PRIVMSG " ++ ch ++ " :" ++ msg
handleReaction h (SendCommand cmd) = handleCommand h cmd -- just to save typing
handleReaction h (Debug msg) = hPutStrLn stderr msg

handleCommand :: Connection -> IRCCommand -> IO ()
handleCommand h (Pong arg) = hPutStrLn h $ "PONG :" ++ arg
handleCommand h (Join ch) = hPutStrLn h $ "JOIN :" ++ ch
handleCommand h (RawCommand cmd) = hPutStrLn h cmd
