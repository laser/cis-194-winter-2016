{-# LANGUAGE OverloadedStrings #-}

module Homework.Week11.Reference where

import Control.Monad (void)
import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, Only(..), query, execute_, execute,
   lastInsertRowId, query_, open, close)

import Homework.Week11.Types

dbName :: String
dbName = "week11.sqlite"

insertUser :: FullName -> EmailAddress -> IO UserId
insertUser fn ea = do
  conn <- open dbName
  execute conn
    " INSERT INTO `users` \
    \ (full_name, email_address) \
    \ VALUES (?, ?)"
    (fn, ea)
  close conn
  lastInsertRowId conn >>= return . UserId

getUsers :: IO [User]
getUsers = do
  conn <- open dbName
  query_ conn
    " SELECT id, full_name, email_address \
    \ FROM `users`"

deleteUserById :: UserId -> IO ()
deleteUserById userId = do
  conn <- open dbName
  execute conn
    " DELETE FROM `users` \
    \ WHERE id = ?"
    (Only userId)
  close conn

getUserById :: UserId -> IO (Maybe User)
getUserById userId = do
  conn <- open dbName
  rs <- query conn
    " SELECT id, full_name, email_address \
    \ FROM `users` \
    \ WHERE id = ?"
    (Only userId)
  close conn
  case rs of
    [] -> return Nothing
    [user] -> return (Just user)

setupDB :: IO ()
setupDB = do
  conn <- open dbName
  execute_ conn "DROP TABLE IF EXISTS `users`"
  execute_ conn
    " CREATE TABLE IF NOT EXISTS `users` \
    \ ( id INTEGER PRIMARY KEY \
    \ , full_name VARCHAR NOT NULL \
    \ , email_address VARCHAR NOT NULL) "
  close conn
