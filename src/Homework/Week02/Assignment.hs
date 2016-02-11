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
import Debug.Trace

-- #1a
parseMessage :: String -> LogMessage
parseMessage s =  case words s of
                        ("E" : code : t : xs) -> LogMessage (Error (read code)) (read t) (unwords xs)
                        ("I" : t : xs) -> LogMessage Info (read t) (unwords xs)
                        ("W" : t : xs) -> LogMessage Warning (read t) (unwords xs)
                        _ -> Unknown s

-- trace ("s is " ++ s)
-- #1b
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) currentTree = currentTree
insert message Leaf = Node Leaf message Leaf
insert newMsg@(LogMessage _ newTime _) (Node lTree msg@(LogMessage _ time _) rTree)
    | newTime > time = Node lTree msg (insert newMsg rTree)
    | otherwise = Node (insert newMsg lTree) msg rTree

-- #3
build :: [LogMessage] -> MessageTree
build = buildReverse . reverse

buildReverse :: [LogMessage] -> MessageTree
buildReverse [] = Leaf
buildReverse (x:xs) = insert x (buildReverse xs)

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lTree msg rTree) = (inOrder lTree) ++ [msg] ++ (inOrder rTree)

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map toMessage . filter wrong . sort
    where   sort = inOrder . build
            wrong (LogMessage (Error severity) _ _) = severity >= 50
            wrong _ = False
            toMessage (LogMessage _ _ message) = message