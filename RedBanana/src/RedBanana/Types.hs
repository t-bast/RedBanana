{-# LANGUAGE DeriveGeneric #-}

module RedBanana.Types where

import Data.Aeson
import Data.Text
import GHC.Generics (Generic)

data Transaction = Transaction 
    { blockHash :: Text
    , blockNumber :: Int
    , confirmations :: Int
    , from :: Text
    , to :: Text
    , value :: Int } 
    deriving (Generic, Show)

instance FromJSON Transaction