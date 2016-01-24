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
parseMessage msg = case words msg of
  ("I":num:details) -> LogMessage Info (read num) (unwords details)
  ("W":num:details) -> LogMessage Warning (read num) (unwords details)
  ("E":severity:num:details) -> LogMessage (Error $ read severity) (read num) (unwords details)
  _ -> Unknown "This is not in the right format"

-- #1b
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree = msgTree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg@(LogMessage _ timeStamp0 _) msgTree@(Node before midLogMsg@(LogMessage _ timeStamp1 _) after)
  | timeStamp0 <= timeStamp1 = Node (insert logMsg before) midLogMsg after
  | otherwise                = Node before                 midLogMsg (insert logMsg after)

-- #3
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node before midLog after) = inOrder before ++ [midLog] ++ inOrder after

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\(LogMessage _ _ details) -> details) . filter (bySeverity (>= 50)) . inOrder . build

bySeverity :: (Int -> Bool) -> LogMessage -> Bool
bySeverity f (LogMessage (Error severity) _ _) = f severity
bySeverity _ _ = False
