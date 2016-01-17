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
parseMessage message = case message of
  ('I':s) -> LogMessage Info (read (head (words s))::Int) (unwords (drop 1 (words s)))
  ('W':s) -> LogMessage Warning (read (head (words s))::Int) (unwords (drop 1 (words s)))
  ('E':s) -> LogMessage (Error (read (head (words s))::Int)) (read (head (drop 1 (words s)))::Int) (unwords (drop 2 (words s)))
  _       -> Unknown message

-- #1b
parse :: String -> [LogMessage]
parse messageLog = map parseMessage (lines messageLog)

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf     = Node Leaf message Leaf
insert message@(LogMessage mtype insertTime rest) tree@(Node left contents@(LogMessage _ treeTime _) right)
  | insertTime <= treeTime  = Node (insert message left) contents right
  | otherwise               = Node left contents (insert message right)

-- #3
build :: [LogMessage] -> MessageTree
build messages = case messages of
  [m]    -> insert m Leaf
  (m:ms) -> insert m (build ms)
  _      -> Leaf

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder = undefined

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
