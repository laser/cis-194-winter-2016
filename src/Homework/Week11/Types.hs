{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Homework.Week11.Types where

import           Data.Int                         (Int64)
import           Data.Text                        (Text)
import           Database.SQLite.Simple           (FromRow (..), field)
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.ToField   (ToField)

newtype UserId = UserId Int64 deriving (Eq, Show, ToField, FromField)
newtype FullName = FullName Text deriving (Eq, Show, ToField, FromField)
newtype EmailAddress = EmailAddress Text deriving (Eq, Show, ToField, FromField)

data User = User { userId           :: UserId
                 , userFullName     :: FullName
                 , userEmailAddress :: EmailAddress } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
