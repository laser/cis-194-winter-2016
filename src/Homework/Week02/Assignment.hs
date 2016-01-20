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
parseMessage (s) =  (case words(s) of
                        ("E" : code : time : xs) -> LogMessage (Error (read code)) (read time) (unwords xs)
                        ("I" : time : xs) -> LogMessage Info (read time) (unwords xs)
                        xs -> Unknown (unwords xs))

-- trace ("s is " ++ s)
-- #1b
parse :: String -> [LogMessage]
parse = parseLines . lines

parseLines :: [String] -> [LogMessage]
parseLines (x:[]) = parseMessage(x) : []
parseLines (x:xs) = parseMessage(x) : parseLines(xs)

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) currentTree = currentTree
insert message Leaf = Node Leaf message Leaf
insert newMessage@(LogMessage _ newTime _) (Node leftTree message@(LogMessage _ time _) rightTree)
    | newTime > time =
        Node leftTree message (insert newMessage rightTree)
    | otherwise =
        Node (insert newMessage leftTree) message rightTree

-- #3
build :: [LogMessage] -> MessageTree
build = buildReverse . reverse

buildReverse :: [LogMessage] -> MessageTree
buildReverse [] = Leaf
buildReverse (x:[])= insert x Leaf
buildReverse (x:xs) = insert x (buildReverse xs)

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf message rightTree) = message : (inOrder rightTree)
inOrder (Node leftTree message rightTree) = (inOrder leftTree) ++ [message] ++ (inOrder rightTree)

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = filterLog . inOrder . build

filterLog :: [LogMessage] -> [String]
filterLog [] = []
filterLog (x@(LogMessage (Error severity) _ message):xs)
    | severity > 49 = message : filterLog(xs)
    | otherwise = filterLog(xs)
filterLog (x:xs) = filterLog xs

