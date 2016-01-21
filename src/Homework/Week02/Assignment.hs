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
    ("W" : ts : msg)         -> LogMessage Warning (read ts) (unwords msg)
    ("I" : ts : msg)         -> LogMessage Info (read ts) (unwords msg)
    ("E" : error : ts : msg) -> LogMessage (Error (read error)) (read ts) (unwords msg)
    otherwise                -> Unknown s

-- #1b
parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

-- #2
when :: LogMessage -> Int
when (LogMessage _ ts _) = ts

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert newMsg (Node left msg right) = 
    if when(newMsg) < when(msg)
        then Node (insert newMsg left) msg right
        else Node left msg (insert newMsg right)

-- #3
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf 

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right)  = inOrder(left) ++ [msg] ++ inOrder(right)

-- #5
msg :: LogMessage -> String
msg (LogMessage _ _ msg) = msg

error50 :: LogMessage -> Bool
error50 (LogMessage (Error code) _ msg) = code >= 50
error50 _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map msg) . (filter error50) . inOrder . build
