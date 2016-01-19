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

toTimeStamp :: String -> TimeStamp
toTimeStamp  = read  

toMsg        :: MessageType -> [String] -> LogMessage
toMsg        msgType (time : xs)  = LogMessage msgType (toTimeStamp time) (unwords xs)

parseInfo    ::  [String] -> LogMessage
parseInfo    =  toMsg Info

parseWarning :: [String] -> LogMessage
parseWarning  = toMsg Warning 

parseError   :: [String] -> LogMessage
parseError (error : xxs  )   = toMsg(Error errCode) xxs
   where errCode = read error :: Int

-- #1a
parseMessage :: String -> LogMessage
parseMessage line = case level of
  "I" -> parseInfo    xs
  "W" -> parseWarning xs
  "E" -> parseError   xs
  _   -> Unknown line
  where (level : xs ) = words line
                    
-- #1b
parse :: String -> [LogMessage]
parse = map parseMessage .lines 

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown s) tree  = tree
insert logMsg      Leaf  = Node Leaf logMsg Leaf
insert logMsg@(LogMessage _ logTime _ )
           tree@(Node left  visitedLogMsg@(LogMessage _ visitedTime _ ) right)
       | logTime < visitedTime  = Node (insert logMsg left) visitedLogMsg  right
       | logTime > visitedTime  = Node  left                visitedLogMsg (insert logMsg right)
       | otherwise              = tree

-- #3
build :: [LogMessage] -> MessageTree
build  = foldl (flip insert) Leaf 

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftSubTree visit rightSubTree) =
  (inOrder leftSubTree) ++ [visit] ++ (inOrder rightSubTree)

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\ (LogMessage  _ _ msg ) -> msg ) .
                 filter (\ (LogMessage logMsg  _  _ ) -> 
                          case logMsg of
                            Error n -> n > 50
                            _       -> False ) . inOrder . build
