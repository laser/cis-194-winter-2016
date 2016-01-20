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

-- #1a
parseMessage :: String -> LogMessage
parseMessage s = case words s of
  "I" : timeStamp : message -> LogMessage Info (read $ timeStamp) (unwords $ message)
  "W" : timeStamp : message -> LogMessage Warning (read $ timeStamp) (unwords $ message)
  "E" : level : timeStamp : message -> LogMessage (Error (read $ level)) (read $ timeStamp) (unwords $ message)
  _ -> Unknown s

-- #1b
parse :: String -> [LogMessage]
parse lns = map parseMessage (lines(lns))

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
