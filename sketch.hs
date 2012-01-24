-- splirc main file


import Control.Monad
import Network
import System.IO
import System.Environment

import Splirc.Types
import Splirc.Parser
import Splirc.ModuleAPI
port = PortNumber 6667


-- main function:
-- connects to the IRC channel <program param 1> using the nickname <program param 2>
-- ! to do: what happens if splirc command "user" fails?
main = do
    hSetBuffering stdout NoBuffering

    handlers <- runSetup
    putStrLn $ "Have " ++ show (length handlers) ++ " handlers"

    putStrLn $ "Connecting ..."
    -- we don't hardcode that to ease testing
    host:nick:args <- getArgs

    h <- connectTo host port
    hSetBuffering h NoBuffering
    hSetNewlineMode h universalNewlineMode
    hPutStrLn h $ "user splirc _ _ :Splirc Haskell Bot"
    hPutStrLn h $ "nick " ++ nick

    let st = State { st_nick=nick, st_conn=h, st_handlers=handlers }

    event st ConnectEvent
	
    readLines h st
    putStrLn $ "stopping Bot"

-- main loop:
-- recursively read line by line. For every Line: parse it -> handle it
readLines:: Handle -> State -> IO ()
readLines h st = do
  t <- hGetLine h 			-- get next line
  putStrLn $ "< " ++ t				--(output line to console window)
  let fromServer = parseString t 	-- parse it
  handleFromServer st fromServer	-- handle it
  readLines h st

-- run the setup methods of all modules, return all event handlers.
-- to do: load modules dynamically
runSetup :: IO [EventHandler] -- IO because setup methods may be IO.
runSetup = (liftM concat . sequence) setups
-- this is basically concat, but with [IO [a]] -> IO [a] instead of [[a]]->[a]
