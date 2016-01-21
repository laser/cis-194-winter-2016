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
-- import Data.List (foldl')

-- #1a
parseMessage :: String -> LogMessage
parseMessage line = case words line of
    ("E" : severity : timeStamp : message) ->
        LogMessage (Error (parseInt severity)) (parseInt timeStamp) (unwords message)
    ("W" : timeStamp : message) ->
        LogMessage Warning (parseInt timeStamp) (unwords message)
    ("I" : timeStamp : message) ->
        LogMessage Info (parseInt timeStamp) (unwords message)
    _ -> Unknown line
    where
        parseInt string = read string :: Int

-- #1b
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert newLogMessage Leaf      = Node Leaf newLogMessage Leaf
insert newLogMessage@(LogMessage _ newTimeStamp _) (Node leftChild logMessage@(LogMessage _ timeStamp _) rightChild)
    | newTimeStamp < timeStamp = Node (insert newLogMessage leftChild) logMessage rightChild
    | otherwise                = Node leftChild logMessage (insert newLogMessage rightChild)


-- #3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf . reverse
-- build = foldl' (flip insert) Leaf

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftChild logMessage rightChild) = (inOrder leftChild) ++ [logMessage] ++ (inOrder rightChild)

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map toMessage . filter isSevereError . inOrder . build
    where
        isSevereError (LogMessage (Error severity) _ _) = severity >= 50
        isSevereError _                                 = False
        toMessage (LogMessage _ _ message)              = message

