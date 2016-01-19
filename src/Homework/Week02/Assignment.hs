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
parseMessage (s) =  trace ("s is %%%%%%%%" ++ s) (case words(s) of
                                            ("E":xs) -> LogMessage (Error 2) 562 "help help"
                                            ("I":xs) -> LogMessage Info 29 "la la la"
                                            _ -> Unknown "This is not in the right format")


-- #1b
parse :: String -> [LogMessage]
parse = undefined

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
