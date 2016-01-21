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
parseMessage message = case messageType of
    "E" ->  let (severity : timestamp : content) = remainingFields
            in LogMessage (Error (parseInt severity)) (parseInt timestamp) (unwords content)
    "W" ->  let (timestamp : content) = remainingFields
            in LogMessage Warning (parseInt timestamp) (unwords content)
    "I" ->  let (timestamp : content) = remainingFields
            in LogMessage Info (parseInt timestamp) (unwords content)
    _   ->  Unknown message
    where
        (messageType : remainingFields) = words message
        parseInt string = read string :: Int

-- #1b
parse :: String -> [LogMessage]
parse string = map parseMessage $ lines string

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert newLogMessage Leaf = Node Leaf newLogMessage Leaf
insert newLogMessage@(LogMessage _ newTimeStamp _) (Node leftChild logMessage@(LogMessage _ timeStamp _) rightChild)
    | newTimeStamp < timeStamp  = Node (insert newLogMessage leftChild) logMessage rightChild
    | otherwise                 = Node leftChild logMessage (insert newLogMessage rightChild)


-- #3
build :: [LogMessage] -> MessageTree
build logMessages = foldr insert Leaf $ reverse logMessages
-- build = foldl' (flip insert) Leaf

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftChild logMessage rightChild) = (inOrder leftChild) ++ [logMessage] ++ (inOrder rightChild)

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong []            = []
whatWentWrong logMessages   = map toMessage $ filter isSevereError $ inOrder $ build logMessages
    where
        isSevereError (LogMessage (Error severity) _ _) = severity >= 50
        isSevereError _                                 = False
        toMessage (LogMessage _ _ message)              = message

