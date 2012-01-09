module Splirc.Modules.Pong where
import Data.Char
import Splirc.Types


setup :: IO [EventHandler]
setup = return [OnPing pongHandler]

pongHandler (PingEvent resp) = return [SendCommand $ Pong $ resp]
