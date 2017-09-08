module RedBanana.Etherscan (getTransactions) where

import Control.Lens ((^.), (.~), (&), (^?))
import Data.Aeson (Value(..), (.:))
import Data.Aeson.Lens (key, _Integer, _String, _Array)
import Data.Aeson.Types (Parser, parseMaybe, parse, Result(..))
import Data.Text (Text)
import Network.Wreq
import Data.Vector (toList)
import RedBanana.Types

asInt :: String -> Integer
asInt = read

toTransaction :: Value -> Parser Transaction
toTransaction (Object v) =
    Transaction
    <$> v .: "blockHash"
    <*> (asInt <$> v .: "blockNumber")
    <*> (asInt <$> v .: "confirmations")
    <*> v .: "from"
    <*> v .: "to"
    <*> (asInt <$> v .: "value")

getTransactions :: Text -> IO (Maybe [Transaction])
getTransactions address = do
    let query = defaults & param "address" .~ [address]
    r <- getWith query "http://api.etherscan.io/api?module=account&action=txlist&startblock=0&endblock=99999999&sort=asc"
    let tx = r ^. responseBody . key "result" . _Array
    return $ mapM (parseMaybe toTransaction) $ toList tx