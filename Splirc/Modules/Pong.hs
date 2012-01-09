module Splirc.Modules.Pong where
import Data.Char
import Splirc.Types


setup :: IO [EventHandler]
setup = do putStrLn "pongSetup" ; return [OnPing pongHandler]

pongHandler (PingEvent resp) = do
  putStrLn "pongHandler called!!"
  return [SendCommand $ Pong $ resp]
