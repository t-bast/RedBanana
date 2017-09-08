{-# LANGUAGE DeriveGeneric #-}

module RedBanana.Types where

import Data.Aeson
import Data.Text
import GHC.Generics (Generic)

data Transaction = Transaction 
    { blockHash :: Text
    , blockNumber :: Text
    , confirmations :: Text
    , from :: Text
    , to :: Text
    , value :: Text } 
    deriving (Generic, Show)

instance FromJSON Transaction