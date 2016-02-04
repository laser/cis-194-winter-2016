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
parseMessage str = case (words str) of
  ("E":code:ts:msg) -> LogMessage (Error (read code)) (read ts) (unwords msg)
  ("I":ts:msg)      -> LogMessage Info (read ts) (unwords msg)
  _                 -> Unknown str


-- #1b
parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert lm@(Unknown str) tree = tree
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ ts str) mt@(Node left x@(LogMessage _ y str2) right) =
  case (compare ts y) of
    EQ -> mt
    GT -> Node left x (insert lm right)
    LT -> Node (insert lm left) x right

-- #3
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left x right) = (inOrder left) ++ [x] ++ (inOrder right)

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map(\(LogMessage _ _ str) -> str) . filter (\(LogMessage mType _ _) -> case mType of
  Error n -> n > 50
  _       -> False) . inOrder . build
