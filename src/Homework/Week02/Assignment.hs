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
parseMessage line = case words line of
        ("E":level:ts:msg)  -> LogMessage (Error (read level :: Int)) (read ts :: TimeStamp) (unwords msg)
        ("I":ts:msg)        -> LogMessage Info (read ts :: TimeStamp) (unwords msg)
        ("W":ts:msg)        -> LogMessage Warning (read ts :: TimeStamp) (unwords msg)
        _                   -> Unknown line


-- #1b
parse :: String -> [LogMessage]
parse log = map parseMessage (lines log)


-- #2 <BST>
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown line) tree          = tree
insert lm@(LogMessage _ _ _) Leaf   = Node Leaf lm Leaf
insert lm@(LogMessage _ l1 _) (Node left nlm@(LogMessage _ l2 _) right) = 
    if l1 < l2 then Node (insert lm left) nlm right else Node left nlm (insert lm right) 
 

-- #3
build :: [LogMessage] -> MessageTree
build (lm:[]) = insert lm Leaf
build (lm:xs) = let node = insert lm Leaf in foldl (\tree el -> insert el tree) node xs  


-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder (Leaf)                  = []
inOrder (Node Leaf lm Leaf)     = [lm]
inOrder (Node left lm right)    = inOrder left ++ (lm : inOrder right)


-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lm = [msg | (LogMessage (Error level) _ msg) <- inOrder (build lm), level > 50]
