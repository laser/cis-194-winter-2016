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

{-
data MessageType = Info
                 | Warning
                 | Error Int

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
-}

import Homework.Week02.Log

-- #1a
parseMessage :: String -> LogMessage
parseMessage message = createLogMessage (words message)

createLogMessage :: [String] -> LogMessage
createLogMessage(messageType:maybeErrorCode:maybeTimeStamp:remaining) = case messageType of
  "E" -> LogMessage (Error (read maybeErrorCode)) (read maybeTimeStamp) (unwords remaining)
  "I" -> LogMessage Info (read maybeErrorCode) (unwords (maybeTimeStamp:remaining))
  "W" -> LogMessage Warning (read maybeErrorCode) (unwords (maybeTimeStamp:remaining))
  _ -> Unknown "This is not in the right format"

-- #1b
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert logMessage existingTree = case logMessage of
  Unknown _ -> existingTree
  _ -> leftOrRight logMessage existingTree

leftOrRight :: LogMessage -> MessageTree -> MessageTree
leftOrRight logMessage@(LogMessage _ newTime _) existingTree = case existingTree of
  Leaf -> Node Leaf logMessage Leaf
  Node left message@(LogMessage _ existingTime _) right -> if newTime < existingTime
    then Node (insert logMessage left) message right
    else Node left message (insert logMessage right)

-- #3
build :: [LogMessage] -> MessageTree
build logList = foldr insert Leaf (reverse logList)

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right
inOrder Leaf = []

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessages = filter (/="") (map whatWentWrongSorted (inOrder (build logMessages)))

whatWentWrongSorted :: LogMessage -> String
whatWentWrongSorted error@(LogMessage (Error _) _ _) = whatWentWrongSortedErrors error
whatWentWrongSorted _ = ""

whatWentWrongSortedErrors :: LogMessage -> String
whatWentWrongSortedErrors (LogMessage (Error severity@_) timeStamp message) = if severity > 50
  then message
  else ""
