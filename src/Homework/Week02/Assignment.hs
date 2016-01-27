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
parseMessage line = case words line of
  ("E" : level : timestamp : message) ->
    LogMessage (Error $ read level) (read timestamp) (unwords message)
  ("W" : timestamp : message) ->
    LogMessage Warning (read timestamp) (unwords message)
  ("I" : timestamp : message) ->
    LogMessage Info (read timestamp) (unwords message)
  _ ->
    Unknown line

-- #1b
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert message@LogMessage{} Leaf = Node Leaf message Leaf
insert new@(LogMessage _ newTimestamp _) (Node left current@(LogMessage _ currentTimestamp _) right)
  | newTimestamp < currentTimestamp = Node (insert new left) current right
  | otherwise                       = Node left current (insert new right)
insert _ tree = tree

-- #3
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right
inOrder Leaf                      = []

-- #5
sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = inOrder . build

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error x) _ _) | x >= 50 = True
isSevereError _                                    = False

logMessageBody :: LogMessage -> String
logMessageBody (LogMessage _ _ body) = body
logMessageBody (Unknown body)        = body

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map logMessageBody . filter isSevereError . sortMessages
