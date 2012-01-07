-- samuel:


module Splirc.Parser where

{- parseString :: String
parseString str = functionFromStrings $ splitString str

splitString :: String -> [String]
splitString []=[]
splitString (x:xs)=if x==' ' then "":(splitString xs) else x:(take 1 restlist):(drop 1 restlist)
  where
    restlist=splitString xs

--
--functionFromStrings :: [String]
--functionFromStrings []=
--functionFromStrings (x:xs)

type Src= String
type Dest= String
type Msg= String
type Params = [String]
functionFromTokens :: Src -> Dest -> Msg -> Params -}





-- marian:
data Command = ServerCommand ServerName Command Params | NickCommand Nick Command Params | PureCommand Command Params
type ServerName = String
type NickName = String
type Command = String
type Params = [String]



handleCommand :: Command -> IO [Reaction]
handleCommand (NickCommand nick "JOIN" [channelname]) = if nick == st_nick st
    then event $ SelfJoinEvent nick channelname
    else event $ JoinEvent nick channelname
handleCommand (NickCommand nick "PART" [channelname]) = if nick == st_nick st
    then event $ SelfPartEvent nick channelname
    else event $ PartEvent nick channelname


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

