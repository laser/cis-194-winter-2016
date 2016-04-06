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

ask :: Monad m => ReaderT r m r
ask = ReaderT return

lift :: Monad m => m a -> ReaderT r m a
lift ma = ReaderT (const ma)

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT g) = ReaderT $ \r -> fmap f (g r)

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT $ const (pure a)
  f <*> g = ReaderT $ \r -> runReaderT f r <*> runReaderT g r

instance Monad m => Monad (ReaderT r m) where
  return = pure
  f >>= g = ReaderT $ \r -> runReaderT f r >>= \a -> runReaderT (g a) r

----------- database lib

insertUser :: FullName -> EmailAddress -> ReaderT Connection IO UserId
insertUser fn ea = do
  conn <- ask
  lift $ execute
    conn
    " INSERT INTO `users` \
    \ (full_name, email_address) \
    \ VALUES (?, ?)"
    (fn, ea)
  lift $ lastInsertRowId conn >>= return . UserId

getUsers :: ReaderT Connection IO [User]
getUsers = do
  conn <- ask
  lift $ query_
    conn
    " SELECT id, full_name, email_address \
    \ FROM `users`"

deleteUserById :: UserId -> ReaderT Connection IO ()
deleteUserById userId = do
  conn <- ask
  _ <- lift $ execute
    conn
    " DELETE FROM `users` \
    \ WHERE id = ?"
    (Only userId)
  return ()

getUserById :: UserId -> ReaderT Connection IO (Maybe User)
getUserById userId = do
  conn <- ask
  rs <- lift $ query
    conn
    " SELECT id, full_name, email_address \
    \ FROM `users` \
    \ WHERE id = ?"
    (Only userId)
  case rs of
    [] -> return Nothing
    user:_ -> return (Just user)

setupDB :: ReaderT Connection IO ()
setupDB = do
  conn <- ask
  lift $ execute_ conn "DROP TABLE IF EXISTS `users`"
  lift $ execute_ conn
    " CREATE TABLE IF NOT EXISTS `users` \
    \ ( id INTEGER PRIMARY KEY \
    \ , full_name VARCHAR NOT NULL \
    \ , email_address VARCHAR NOT NULL) "
