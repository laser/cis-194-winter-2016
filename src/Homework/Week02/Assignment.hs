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
errorMsg :: String -> LogMessage
errorMsg s = LogMessage error ts msg
    where tokens = words s
          errCode = read(tokens !! 1)
          error = Error errCode
          ts = read(tokens !! 2)
          msg = unwords $ drop 3 tokens

makeMsg :: MessageType -> String -> LogMessage
makeMsg t s = LogMessage t ts msg
    where tokens = words s
          ts = read(tokens !! 1)
          msg = unwords $ drop 2 tokens

infoMsg :: String -> LogMessage
infoMsg = makeMsg Info

warnMsg :: String -> LogMessage
warnMsg = makeMsg Warning

parseMessage :: String -> LogMessage
parseMessage s@('W':' ':_) = warnMsg s
parseMessage s@('E':' ':_) = errorMsg s
parseMessage s@('I':' ':_) = infoMsg s
parseMessage s = Unknown s

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
build = foldl (\ tree msg -> insert msg tree) Leaf 

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
