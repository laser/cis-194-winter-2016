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

parseMessage :: String -> LogMessage
parseMessage (message)
    | head ws == ['E'] = LogMessage (Error (read (ws !! 1)::Int)) (read (ws !! 2)) (unwords (drop 3 ws))
    | head ws == ['I'] = LogMessage Info (read (ws !! 1)::Int) (unwords (drop 2 ws))
    | head ws == ['W'] = LogMessage Warning (read (ws !! 1)::Int) (unwords (drop 2 ws))
    | otherwise = Unknown "This is not in the right format"
    where ws = words (message)

-- #1b
parse :: String -> [LogMessage]
parse (multiLineLog) = map parseMessage (lines(multiLineLog))

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert (toBeAdded@(LogMessage msgType timeToBeAdded msgString)) (Node l root@(LogMessage _ time _) r) =
    if (timeToBeAdded < time)
      then Node (insert (toBeAdded) l) root r
    else
      Node l root (insert (toBeAdded) r)

-- #3
build :: [LogMessage] -> MessageTree
build logMessages = foldl (flip insert) Leaf logMessages

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

isRelevantError :: LogMessage -> Bool
isRelevantError (LogMessage (Error severity) _ _)
    | severity >= 50 = True
    | otherwise = False
isRelevantError _  = False

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessages =
  let relevantErrorMessages = filter (isRelevantError) logMessages
      messageTree = build relevantErrorMessages
      orderedLogMessages = inOrder messageTree
      messages = map (\msg@(LogMessage msgType timeToBeAdded msgString) -> msgString) orderedLogMessages
  in messages
