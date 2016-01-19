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
import Debug.Trace

-- #1a
parseMessage :: String -> LogMessage
parseMessage (s) =  (case words(s) of
                        ("E" : code : time : xs) -> LogMessage (Error (read code)) (read time) (unwords xs)
                        ("I" : time : xs) -> LogMessage Info (read time) (unwords xs)
                        xs -> Unknown (unwords xs))

-- trace ("s is " ++ s)
-- #1b
parse :: String -> [LogMessage]
parse = parseLines . lines

parseLines :: [String] -> [LogMessage]
parseLines (x:[]) = parseMessage(x) : []
parseLines (x:xs) = parseMessage(x) : parseLines(xs)

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert = undefined

-- #3
build :: [LogMessage] -> MessageTree
build = undefined

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder = undefined

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
