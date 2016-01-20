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
  ("W":timestamp:message) -> LogMessage Warning (read timestamp::Int) (unwords message)
  ("I":timestamp:message) -> LogMessage Info (read timestamp::Int) (unwords message)
  ("E":level:timestamp:message) -> LogMessage (Error (read level::Int)) (read timestamp::Int) (unwords message)
  (_) -> Unknown s

-- #1b
parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message (Node a rootMessage b)
  | older message rootMessage = Node (insert message a) rootMessage b
  | otherwise                 = Node a rootMessage (insert message b)

older :: LogMessage -> LogMessage -> Bool
older (LogMessage _ ts1 _) (LogMessage _ ts2 _) = ts1 < ts2

-- #3
build :: [LogMessage] -> MessageTree
build messages = foldr insert Leaf (reverse messages)

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = (inOrder left) ++ [message] ++ (inOrder right)

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = map (\(LogMessage _ _ m) -> m) (inOrder $ build $ filter isRelevantError ms)

isRelevantError :: LogMessage -> Bool
isRelevantError (LogMessage (Error severity) _ _) = severity > 50
isRelevantError _ = False
