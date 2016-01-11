module Homework.Week02.Assignment (
  build,
  inOrder,
  insert,
  parse,
  parseMessage,
  parseErrorMessage,
  timeStamp,
  whatWentWrong,
  LogMessage(..),
  MessageTree(..),
  MessageType(..),
  TimeStamp
) where

import Homework.Week02.Log

-- #1a

--for Errors: "E 2 562 yo, dawg"
-- cain't be here, if first isn't 'E'
parseErrorMessage :: String -> LogMessage
parseErrorMessage s =
  let (_:ecd:emsg:rest) = words s
      cd = read ecd :: Int
      cmsg = read emsg :: Int
      str = unwords rest
  in LogMessage (Error cd) cmsg str

-- all non error type messages
parseNonErrorMessage :: String -> LogMessage
parseNonErrorMessage s =
  let (t:msg:rest) = words s
      msg' = read msg :: Int
      str = unwords rest
      type' = case t of
        "I" -> Info
        "W" -> Warning
  in LogMessage type' msg' str

parseMessage :: String -> LogMessage
parseMessage s = case s of

  ('E' : rest) -> parseErrorMessage s
  ('I' : rest) -> parseNonErrorMessage s
  ('W' : rest) -> parseNonErrorMessage s
  _ -> Unknown s

-- #1b
parse :: String -> [LogMessage]
parse = map parseMessage . lines

timeStamp :: LogMessage -> TimeStamp
timeStamp (LogMessage _ ts _) = ts
timeStamp _ = 0

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) _ = Leaf
insert (LogMessage mt ts s) Leaf = Node Leaf (LogMessage mt ts s) Leaf
--insert (LogMessage mt ts s) mtree = case mtree

-- assume a sorted messageTree to start. if you're inserting an Unknown LogMessage,
-- just return an empty [Leaf MT]
-- If you're inserting a 'real' LogMessage [with it's ts], try to figure out
-- where to insert it. It should be larger that the one on the left and
-- smaller than the one on the right should go to the right of the the node with the LogMessage's ts tha
-- starting with the left [smallest] node, ask whether your ts is bigger
-- than the leftMessage, AND is it bigger than


-- #3
build :: [LogMessage] -> MessageTree
build = undefined

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder = undefined

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
