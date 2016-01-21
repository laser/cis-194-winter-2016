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
  "I" : timeStamp : message -> LogMessage Info (read $ timeStamp) (unwords $ message)
  "W" : timeStamp : message -> LogMessage Warning (read $ timeStamp) (unwords $ message)
  "E" : level : timeStamp : message -> LogMessage (Error (read $ level)) (read $ timeStamp) (unwords $ message)
  _ -> Unknown s

-- #1b
parse :: String -> [LogMessage]
parse lns = map parseMessage (lines(lns))

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown s) tree = tree
insert newMsg Leaf = Node Leaf newMsg Leaf
insert (LogMessage newType newTs newText) (Node leftNode (LogMessage oldType oldTs oldText) rightNode) =
        if (newTs < oldTs)
                then Node (insert (LogMessage newType newTs newText) leftNode) (LogMessage oldType oldTs oldText) rightNode
                else Node leftNode (LogMessage oldType oldTs oldText) (insert (LogMessage newType newTs newText) rightNode)

-- #3
build :: [LogMessage] -> MessageTree
build messages = buildImpl (reverse messages)

buildImpl :: [LogMessage] -> MessageTree
buildImpl [ someMessage ] = insert someMessage Leaf
buildImpl (someMessage : moreMessages) = insert someMessage (buildImpl moreMessages)

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder messageTree = case messageTree of
  Leaf -> []
  Node leftNode message rightNode -> inOrder leftNode ++ [ message ] ++ inOrder rightNode

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = [text | LogMessage (Error level) ts text <- messages, level >= 50]
