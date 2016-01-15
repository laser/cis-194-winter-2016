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

-- data LogMessage = LogMessage MessageType TimeStamp String
--                 | Unknown String

--  parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"

-- #1a
parseMessageList :: [String] -> LogMessage
parseMessageList ("E" : messageType : timeStamp : message) = LogMessage (Error (read messageType :: Int)) (read timeStamp :: Int)  (unwords message)
parseMessageList ("I" : messageType : message) = LogMessage Info (read messageType :: Int)  (unwords message)
parseMessageList unknown = Unknown (unwords unknown)

parseMessage :: String -> LogMessage
parseMessage = parseMessageList . words

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
