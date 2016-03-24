{-# LANGUAGE OverloadedStrings #-}

module Homework.Week11Spec (
  main,
  spec
) where

import           Data.Maybe                 (fromJust)
import           Test.Hspec

import           Homework.Week11.Assignment (ReaderT (..), close, dbName,
                                             deleteUserById, getUserById,
                                             getUsers, insertUser, lift, open,
                                             setupDB)
import qualified Homework.Week11.Reference  as R
import           Homework.Week11.Types      (EmailAddress (..), FullName (..),
                                             User (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "week 11" $ do
    it "manages the user lifecycle using the reference implementation" $ do
      pending
      referenceLifecycle

    it "enable the following test (and un-comment it out below) when you are ready" $ do
      pending
      -- lifecycle

-- This set of tests asserts the behavior of our legacy database library. The
-- library opens and closes a connection for each database call, which is not
-- optimal.
referenceLifecycle :: IO ()
referenceLifecycle = do
  -- set up the database
  R.setupDB

  -- ensure no rows already exist
  users <- R.getUsers
  users `shouldBe` []

  -- insert some users
  id1 <- R.insertUser (FullName "Jebb Harrow") (EmailAddress "test0@example.com")
  id2 <- R.insertUser (FullName "Sue Meforit") (EmailAddress "test1@example.com")
  id3 <- R.insertUser (FullName "Yoko Ohnoes") (EmailAddress "test2@example.com")

  -- should have three in there now...
  users <- R.getUsers
  map (userId) users `shouldBe` [id1, id2, id3]

  -- load by id
  u2 <- R.getUserById id2
  fromJust u2 `shouldBe` (User id2 (FullName "Sue Meforit") (EmailAddress "test1@example.com"))

  -- delete by id
  R.deleteUserById id2

  -- now we should have 2
  users <- R.getUsers
  map (userId) users `shouldBe` [id1, id3]

-- This set of tests hits our new database library, which shares a connection
-- across database queries which it reads from its environment (ReaderT). You
-- will need to re-implement the legacy library to use ReaderT Connection IO a.
{-lifecycle :: IO ()-}
{-lifecycle = do-}
  {-conn <- open dbName-}
  {-(flip runReaderT) conn $ do-}
    {--- set up the database-}
    {-setupDB-}

    {--- ensure no rows already exist-}
    {-users <- getUsers-}
    {-lift (users `shouldBe` [])-}

    {--- insert some users-}
    {-id1 <- insertUser (FullName "Jebb Harrow") (EmailAddress "test0@example.com")-}
    {-id2 <- insertUser (FullName "Sue Meforit") (EmailAddress "test1@example.com")-}
    {-id3 <- insertUser (FullName "Yoko Ohnoes") (EmailAddress "test2@example.com")-}

    {--- should have three in there now...-}
    {-users <- getUsers-}
    {-lift (map (userId) users `shouldBe` [id1, id2, id3])-}

    {--- load by id-}
    {-u2 <- getUserById id2-}
    {-lift (fromJust u2 `shouldBe` (User id2 (FullName "Sue Meforit") (EmailAddress "test1@example.com")))-}

    {--- delete by id-}
    {-deleteUserById id2-}

    {--- now we should have 2-}
    {-users <- getUsers-}
    {-lift (map (userId) users `shouldBe` [id1, id3])-}
  {-close conn-}
