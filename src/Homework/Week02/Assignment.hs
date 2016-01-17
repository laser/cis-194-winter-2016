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
import Data.List (foldl')

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

-- helper: grab the ts. if you give it a LogMessage, it'll
-- spit back a ts
timeStamp :: LogMessage -> TimeStamp
timeStamp (LogMessage _ ts _) = ts
timeStamp _ = 0


-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert lm@(LogMessage mt ts str) Leaf = Node Leaf lm Leaf
insert lm@(LogMessage mt ts str) (Node lft lm1@(LogMessage _ ts2 _) rt)
  | ts > ts2 = Node lft lm1 (insert lm rt)
  | ts <= ts2 = Node (insert lm lft) lm1 rt
insert _ mt = mt

-- assume a sorted messageTree to start. if you're inserting an Unknown LogMessage,
-- just return an empty [Leaf MT]
-- If you're inserting a 'real' LogMessage [with it's ts], try to figure out
-- where to insert it. It should be larger that the one on the left and
-- smaller than the one on the right should go to the right of the the node with the LogMessage's ts tha
-- starting with the left [smallest] node, ask whether your ts is bigger
-- than the leftMessage, AND is it bigger than


-- #3
--build :: Foldable t => t LogMessage -> MessageTree
build :: [LogMessage] -> MessageTree
--build = foldr insert Leaf
--build [] = Leaf
--build (x : xs) = insert x (build xs)
build = foldl' (flip insert) Leaf

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = [] -- it stops
inOrder (Node lfs lm rts) =
  inOrder lfs ++ [lm] ++ inOrder rts

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
