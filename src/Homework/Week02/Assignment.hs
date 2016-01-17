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

import           Homework.Week02.Log

-- #1a
parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
  ("I":n:rest)      -> LogMessage Info (read n::Int) (unwords rest)
  ("W":n:rest)      -> LogMessage Warning (read n::Int) (unwords rest)
  ("E":sev:n:rest)  -> LogMessage (Error (read sev::Int)) (read n::Int) (unwords rest)
  (_)               -> Unknown msg

-- #1b
parse :: String -> [LogMessage]
parse messageLog = map parseMessage (lines messageLog)

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf     = Node Leaf message Leaf
insert message@(LogMessage mtype insertTime rest) tree@(Node left contents@(LogMessage _ treeTime _) right)
  | insertTime <= treeTime  = Node (insert message left) contents right
  | otherwise               = Node left contents (insert message right)

-- #3
build :: [LogMessage] -> MessageTree
build messages = case messages of
  [m]    -> insert m Leaf
  (m:ms) -> insert m (build ms)
  _      -> Leaf

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder tree@(Node left contents right)  = (inOrder left) ++ [contents] ++ (inOrder right)

-- #5
errors50AndUp :: LogMessage -> Bool
errors50AndUp (LogMessage (Error sev) _ _) = sev > 49
errors50AndUp _                            = False

getMsg :: LogMessage -> String
getMsg (LogMessage _ _ msg) = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map getMsg) . (filter errors50AndUp) . inOrder . build
