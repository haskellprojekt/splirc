module Splirc.Parser.TextFunctions where

extractNext :: String -> String
extractNext "" = ""
extractNext (' ':xs) = ""
extractNext (x:xs) = x:(extractNext xs)

extractRest :: String -> String
extractRest "" = ""
extractRest (' ':xs) = xs
extractRest (x:xs) = (extractRest xs)

-- does the string contain <token> (at any position)?
contains :: String -> String -> Bool
contains "" "" = True
contains "" str = True
contains token "" = False
contains token@(token_fst:token_rest) (x:xs) =
  if (token_fst==x) && (contains token_rest xs)
  then True
  else contains token xs

startsWith :: String -> String -> Bool
startsWith "" "" = True
startsWith token "" = False
startsWith "" str = True
startsWith (token:token_rest) (str:str_rest) = (token==str) -- && (startsWith token_rest str_rest)

--unpack
removeLeadingRec token str = if (startsWith token str) then (removeLeadingRec token (removeLeading token str)) else str

removeLeading :: String -> String -> String
removeLeading "" str = str
removeLeading token@(token_fst:token_rest) str@(str_fst:str_rest) = if (startsWith token str) then removeLeading token_rest str_rest else str
--removeLeading str = str

-- example: (removeTrailing "asdfjkö    ") == "asdfjkö"

--removeTrailingRec token str = if removeTrailing token str

removeTrailing str=if (drop ((length str)-1) str)==" " then removeTrailing (take ((length str) -1) str) else str