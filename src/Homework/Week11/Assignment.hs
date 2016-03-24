{-# LANGUAGE OverloadedStrings #-}

module Homework.Week11.Assignment (
  ReaderT(..),
  ask,
  lift,
  insertUser,
  getUsers,
  getUserById,
  deleteUserById,
  setupDB,
  open,
  close,
  dbName
) where

import           Data.Text              (Text)
import           Database.SQLite.Simple (Connection, Only (..), close, execute,
                                         execute_, lastInsertRowId, open, query,
                                         query_)

import           Homework.Week11.Types

dbName :: String
dbName = "week11.sqlite"

----------- ReaderT

-- Either use ReaderT from the transformers package
-- or write your own version (for extra credit!)

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
ask = undefined
lift = undefined

----------- database lib

insertUser :: FullName -> EmailAddress -> ReaderT Connection IO UserId
insertUser = undefined

getUsers :: ReaderT Connection IO [User]
getUsers = undefined

deleteUserById :: UserId -> ReaderT Connection IO ()
deleteUserById = undefined

getUserById :: UserId -> ReaderT Connection IO (Maybe User)
getUserById = undefined

setupDB :: ReaderT Connection IO ()
setupDB = undefined
{-setupDB = do-}
  {-conn <- ask-}
  {-lift $ execute_ conn "DROP TABLE IF EXISTS `users`"-}
  {-lift $ execute_ conn-}
    {-" CREATE TABLE IF NOT EXISTS `users` \-}
    {-\ ( id INTEGER PRIMARY KEY \-}
    {-\ , full_name VARCHAR NOT NULL \-}
    {-\ , email_address VARCHAR NOT NULL) "-}
