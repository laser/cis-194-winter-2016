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
insert newMessage@(LogMessage Warning newTime _) (Node leftTree message@(LogMessage Warning time _) rightTree)
    | newTime > time =
        trace("decompose - right " ++ show newMessage ++ " : " ++ show message)
        Node leftTree message (insert newMessage rightTree)
    | otherwise =
        trace("decompose - left " ++ show newMessage ++ " : " ++ show message)
        Node (insert newMessage leftTree) message rightTree

-- #3
build :: [LogMessage] -> MessageTree
build = undefined

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder = undefined

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
