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
    "E" : severity : timeStamp : message -> LogMessage (Error (read severity)) (read timeStamp) (unwords message)
    "W" : timeStamp : message            -> LogMessage Warning (read timeStamp) (unwords message)
    "I" : timeStamp : message            -> LogMessage Info (read timeStamp) (unwords message)
    _                                    -> Unknown line

-- #1b
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert newLogMessage Leaf      = Node Leaf newLogMessage Leaf
insert newLogMessage@(LogMessage _ newTimeStamp _) (Node leftTree logMessage@(LogMessage _ timeStamp _) rightTree)
    | newTimeStamp < timeStamp = Node (insert newLogMessage leftTree) logMessage rightTree
    | otherwise                = Node leftTree logMessage (insert newLogMessage rightTree)


-- #3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf . reverse
-- build = foldl' (flip insert) Leaf

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree logMessage rightTree) = (inOrder leftTree) ++ [logMessage] ++ (inOrder rightTree)

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map toMessage . filter isSevereError . inOrder . build
    where
        isSevereError (LogMessage (Error severity) _ _) = severity >= 50
        isSevereError _                                 = False
        toMessage (LogMessage _ _ message)              = message

