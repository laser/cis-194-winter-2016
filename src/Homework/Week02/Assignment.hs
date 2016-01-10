module Homework.Week02.Assignment (
  build,
  inOrder,
  insert,
  parse,
  parseMessage,
  parseErrorMessage,
  whatWentWrong,
  LogMessage(..),
  MessageTree(..),
  MessageType(..),
  TimeStamp
) where

import Homework.Week02.Log

-- #1a

--for Errors: "E 2 562 yo, dawg"
-- cain't be here, if first isn't 'E'
parseErrorMessage :: String -> LogMessage
parseErrorMessage s =
  let (_:ecd:emsg:rest) = words s
      cd = read ecd :: Int
      cmsg = read emsg :: Int
      str = unwords rest
  in LogMessage (Error cd) cmsg str

-- all non error type messages
parseNonErrorMessage :: String -> LogMessage
parseNonErrorMessage s =
  let (t:msg:rest) = words s
      msg' = read msg :: Int
      str = unwords rest
      type' = case t of
        "I" -> Info
        "W" -> Warning
  in LogMessage type' msg' str

parseMessage :: String -> LogMessage
parseMessage s = case s of

  ('E' : rest) -> parseErrorMessage s
  ('I' : rest) -> parseNonErrorMessage s
  ('W' : rest) -> parseNonErrorMessage s
  _ -> Unknown s

-- #1b
parse :: String -> [LogMessage]
parse = map parseMessage . lines

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
