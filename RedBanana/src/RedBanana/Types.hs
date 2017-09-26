{-# LANGUAGE DeriveGeneric #-}

module RedBanana.Types where

import Data.Aeson
import Data.Text
import GHC.Generics (Generic)

data Transaction = Transaction 
    { hash :: Text
    , blockHash :: Text
    , blockNumber :: Integer
    , confirmations :: Integer
    , from :: Text
    , to :: Text
    , value :: Integer } 
    deriving (Generic, Show)

instance FromJSON Transaction

data TransactionArgs = TransactionArgs
    { address :: Text
    , startBlock :: Int
    , endBlock :: Int
    , sort :: Text }
    deriving (Show)