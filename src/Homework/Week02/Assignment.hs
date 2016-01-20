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
        trace("decompose - right " ++ show newMessage ++ " : " ++ show message)
        Node leftTree message (insert newMessage rightTree)
    | otherwise =
        trace("decompose - left " ++ show newMessage ++ " : " ++ show message)
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
whatWentWrong = undefined
