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
parseMessage message = case messageType of
    "E" ->  let (severity : timestamp : content) = remainingFields
            in LogMessage (Error (parseInt severity)) (parseInt timestamp) (unwords content)
    "W" ->  let (timestamp : content) = remainingFields
            in LogMessage Warning (parseInt timestamp) (unwords content)
    "I" ->  let (timestamp : content) = remainingFields
            in LogMessage Info (parseInt timestamp) (unwords content)
    _   ->  Unknown message
    where
        (messageType : remainingFields) = words message
        parseInt string = read string :: Int

-- #1b
parse :: String -> [LogMessage]
parse = undefined

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
