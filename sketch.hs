-- all together a little ugly, we should clean that up, split it in modules etc


import Control.Monad
import Network
import System.IO
import System.Environment

import Splirc.Types
import Splirc.Parser
import Splirc.ModuleAPI
port = PortNumber 6667

main = do
    hSetBuffering stdout NoBuffering
    handlers <- runSetup
    putStrLn $ "Have " ++ show (length handlers) ++ " handlers"

    putStrLn $ "Connecting ..."
    -- we don't hardcode that to ease testing
    host:nick:args <- getArgs
    h <- connectTo host port
    hSetBuffering h NoBuffering
    hPutStrLn h $ "user splirc _ _ :Splirc Haskell Bot"
    hPutStrLn h $ "nick " ++ nick

    let st = State { st_nick=nick, st_conn=h, st_handlers=handlers }

    event st ConnectEvent
	
    readLines h st
    putStr "stopping Bot"


readLines:: Handle -> State -> IO ()
readLines h st = do
  t <- hGetLine h
  print ("fromServer: "++t)
  let fromServer = parseString t
  putStrLn ("As command: "++ (show fromServer))
  handleFromServer st fromServer
  readLines h st

-- run the setup methods of all modules, return all event handlers.
runSetup :: IO [EventHandler] -- IO because setup methods may be IO.
runSetup = (liftM concat . sequence) setups
-- this is basically concat, but with [IO [a]] -> IO [a] instead of [[a]]->[a]


-- EVENT HANDLING STUFF


