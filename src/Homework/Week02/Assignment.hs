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

--for Info: "I 562 yo, dawg"
-- parseInfoMessage :: String -> LogMessage
-- parseInfoMessage s =
--   let (_:imsg:rest) = words s
--       cmsg = read imsg :: Int
--       str = unwords rest
--   in LogMessage Info cmsg str

-- parseWarningMessage :: String -> LogMessage
-- parseWarningMessage s =
--   let (_:wmsg:rest) = words s
--       cmsg = read wmsg :: Int
--       str = unwords rest
--   in LogMessage Warning cmsg str

parseTypeMessage :: String -> LogMessage
parseTypeMessage s =
  let (t:msg:rest) = words s
      msg' = read msg :: Int
      str = unwords rest
      type' = case t of
        "I" -> Info
        "W" -> Warning
  in LogMessage type' msg' str


  --for Warning: "W 562 yo, dawg"
  -- parseWarnMessage s = undefined
  --   let (_:wmsg:rest) = words s
  --       cmsg = read wmsg :: Int
  --       str = unwords rest
  --   in LogMessage Warning wmsg str

parseMessage :: String -> LogMessage
parseMessage s = case s of

  ('E' : rest) -> parseErrorMessage s
  ('I' : rest) -> parseTypeMessage s
  ('W' : rest) -> parseTypeMessage s
  _ -> Unknown s


--parseMessage (LogMessage MessageType ts str)

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
