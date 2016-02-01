{-# OPTIONS_GHC -Wall #-}

module Homework.Week02.Assignment (
  build,
  inOrder,
  insert,
  parse,
  parseMessage,
  parseErrorMessage,
  whatWentWrong,
  LogMessage(..),
  MessageTree(..),
  MessageType(..)
) where

import Homework.Week02.Log
import Data.List (foldl')

-- #1a

parseErrorMessage :: String -> LogMessage
parseErrorMessage s =
  let (_:ecd:ts:rest) = words s
      cd = read ecd :: Int
      ts' = read ts :: Int
      str = unwords rest
  in LogMessage (Error cd) ts' str

parseNonErrorMessage :: String -> LogMessage
parseNonErrorMessage s =
  let (_:ts:rest) = words s
      ts' = read ts :: Int
      str = unwords rest
  in LogMessage Warning ts' str

parseNonErrorMessage2 :: String -> LogMessage
parseNonErrorMessage2 s =
  let (_:ts:rest) = words s
      ts' = read ts :: Int
      str = unwords rest
  in LogMessage Info ts' str

parseMessage :: String -> LogMessage
parseMessage s = case s of

  ('E' : _) -> parseErrorMessage s
  ('I' : _) -> parseNonErrorMessage2 s
  ('W' : _) -> parseNonErrorMessage s
  _ -> Unknown s

-- #1b
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert lm@(LogMessage _ _ _) Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node lft lm1@(LogMessage _ ts2 _) rt)
  | ts > ts2 = Node lft lm1 (insert lm rt)
  | ts <= ts2 = Node (insert lm lft) lm1 rt
insert _ mt = mt

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


errorValue :: LogMessage -> Bool
errorValue (LogMessage (Error x) _ _)
   | x >= 50 = True
   | otherwise = False
errorValue _ = False

-- #5
whatWentWrong :: [LogMessage] -> [String]
--whatWentWrong lstLms  = map messs $ filter errorValue (inOrder  (build lstLms))
whatWentWrong = map mess . filter errorValue . inOrder . build
   where mess :: LogMessage -> String
         mess (LogMessage _ _ msg) = msg
         mess _ = []
