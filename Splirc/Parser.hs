-- samuel:


module Splirc.Parser where

import Splirc.Types
import Splirc.Parser.TextFunctions

{--parseStringIO :: IO String -> IO FromServer
parseStringIO str= (liftM parseString) str--}

-- parses a message from a IRC-Server, returning it using the internal representation
-- to do: define test case/assertion !
parseString :: String -> FromServer
parseString (':':str) =						--ServerCommands and Nickcommands begin with ":"
  if (is_server_command serverOrNick)
  then ServerCommand serverOrNick command (parseParams params)
  else NickCommand (onlyNick serverOrNick) command (parseParams params)
    where
      serverOrNick = extractNext str
      command = extractNext (extractRest str)
      params = (extractRest (extractRest str))
      is_server_command str = contains "." str && not (contains "@" str || contains "!" str)
      onlyNick = takeWhile (`notElem` "!@")
parseString str = PureCommand command (parseParams params)	--The command must be a Purecommand:
  where
    command = extractNext str
    params = extractRest str

parseParams "" = --error "unterminated parameter List!!!"
  [] -- don' throw an error, to prevent the Bot from crashing. Obviously parameter lists often are not terminated correctly... :-P
parseParams (':':str) = [str]
parseParams str = (extractNext str):(parseParams (extractRest str))

--split a string at any " ", eg.:

--    (splitString dsntMatter "bli bla blubb") == ["bli","bla","blubb"]

-- ignore leading/trailing " "s
--             |
--             V
{-splitString :: Bool -> String -> [String]
splitString True (' ':xs) = splitString True rest where
  rest=removeTrailing xs
splitString True str = splitString False (removeTrailing str)

splitString False ""=[""]
splitString False (x:xs)=if x==' ' then "":(restlist) else (x:(restlist!!0)):(drop 1 restlist)
  where
    restlist=splitString False xs-}

--
{-type Src= String
type Dest= String
type Msg= String
type Params = [String]
functionFromTokens :: Src -> Dest -> Msg -> Params -}







-- message = :servername befehl parameter  ServerCommand servername befehl parameter
-- message = :nickname befehl parameter    NickCommand nickname befehl parameter
-- message = befehl parameter              PureCommand befehl parameter
-- 
-- befehl = buchstaben                     
-- befehl = 123
-- 
-- parameter = :                           []
-- parameter = :letzter                    [letzter]
-- parameter = erster :letzter             [erster, letzter]
-- parameter = erster zweiter :letzter     [erster, zweiter, letzter]

