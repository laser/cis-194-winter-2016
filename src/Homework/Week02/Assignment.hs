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
parseMessageList :: [String] -> LogMessage
parseMessageList ("E" : messageType : timeStamp : message) = LogMessage (Error (read messageType :: Int)) (read timeStamp :: Int)  (unwords message)
parseMessageList ("I" : messageType : message) = LogMessage Info (read messageType :: Int)  (unwords message)
parseMessageList ("W" : messageType : message) = LogMessage Warning (read messageType :: Int)  (unwords message)
parseMessageList unknown = Unknown (unwords unknown)

parseMessage :: String -> LogMessage
parseMessage = parseMessageList . words

-- #1b
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) originalMessageTree  =  originalMessageTree
insert element Leaf = Node Leaf element Leaf
insert e @ (LogMessage _ elementTime _ ) m @ (Node left (logMessage @ (LogMessage _ nodeTime _ )) right) = case elementTime > nodeTime of
                                                                                      True -> Node left logMessage (insert e right)
                                                                                      otherwise -> Node (insert e left) logMessage right
-- #3
build :: [LogMessage] -> MessageTree
build (x:y) = foldr insert (Node Leaf x Leaf) y

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left singleElement right) =  inOrder left ++ singleElement : inOrder right

-- #5
extractSevMessage :: LogMessage -> String
extractSevMessage (LogMessage _ _ message ) = message
extractSevMessage idontknowwhatitis = ""

extractSevLevelOver50 :: LogMessage -> Bool
extractSevLevelOver50 (LogMessage (Error x) _ _ ) = x > 50
extractSevLevelOver50 whatDoYouMean = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractSevMessage . filter extractSevLevelOver50 . inOrder . build
