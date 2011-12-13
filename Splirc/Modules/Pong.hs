module Splirc.Modules.Pong where
import Data.Char
import Splirc.Types


setup :: IO [EventHandler]
setup = return [OnEveryResponse pongHandler]

pongHandler (ResponseEvent resp) =
    if (map toUpper $ take 5 resp) == "PING " then
        return [SendCommand $ Pong $ drop 5 resp]
    else
        return []
