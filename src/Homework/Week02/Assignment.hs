module Homework.Week02.Assignment (
  build,
  inOrder,
  insert,
  parse,
  parseMessage,
  whatWentWrong,
  LogMessage(..),
  MessageTree(..),
  MessageType(..),
  TimeStamp
) where

import Homework.Week02.Log

toTimeStamp :: String -> TimeStamp
toTimeStamp  = read  

parseInfo    ::  [String] -> LogMessage
parseInfo (ts : rest ) =    LogMessage Info    (toTimeStamp ts)  (unwords rest)

parseWarning :: [String] -> LogMessage
parseWarning (ts : rest) =  LogMessage Warning (toTimeStamp ts)  (unwords rest)


parseError   :: [String] -> LogMessage
parseError (error : ts : rest )   =
   LogMessage (Error errCode)
              (toTimeStamp ts)
              (unwords rest)
   where errCode = read error :: Int

-- #1a
parseMessage :: String -> LogMessage
parseMessage line = case level of
  "I" -> parseInfo    xs
  "W" -> parseWarning xs
  "E" -> parseError   xs
  _   -> Unknown line
  where (level : xs ) = words line
                    
-- #1b
parse :: String -> [LogMessage]
parse input = map parseMessage $ lines input

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert = undefined

-- #3
build :: [LogMessage] -> MessageTree
build = undefined

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder = undefined

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
