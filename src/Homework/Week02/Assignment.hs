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
parseMessage message = case (words message) of
    ("I" : ts : em) -> LogMessage Info (read ts) (unwords em)
    ("W" : ts : em) -> LogMessage Warning (read ts) (unwords em)
    ("E" : errorCode : ts : em) -> LogMessage (Error (read errorCode)) (read ts) (unwords em)
    _ -> Unknown "This is not in the right format"

-- #1b
parse :: String -> [LogMessage]
parse fileString = map parseMessage (lines fileString)

-- #2
getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ ts _) = ts 

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logmessage Leaf = Node Leaf logmessage Leaf
insert m@(LogMessage _ ts _) (Node leftTree tlm rightTree) 
  | ts < (getTimeStamp tlm) = Node (insert m leftTree) tlm rightTree
  | ts > (getTimeStamp tlm) = Node leftTree tlm (insert m rightTree)

-- #3
build :: [LogMessage] -> MessageTree
build p@(x : xs)
  | p == [] = Leaf
  | length xs == 0 = Node Leaf x Leaf
  | otherwise =  insert (last p) (build (init p))

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftSubTree lm rightSubTree) = inOrder leftSubTree ++ [lm] ++  inOrder rightSubTree

-- #5
isImportant :: LogMessage -> Bool
isImportant p@(LogMessage (Error severity) _ _)
  | severity >= 50 = True  
  | otherwise  = False
isImportant _ = False

getOrderedImportantMessages :: [LogMessage] -> [LogMessage]
getOrderedImportantMessages messagesList = inOrder (build (filter isImportant (messagesList)))

getString :: LogMessage -> String
getString p@(LogMessage _ _ returnString) = returnString

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong messagesList = map getString (getOrderedImportantMessages messagesList)
