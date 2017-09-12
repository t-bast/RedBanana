module RedBanana.Etherscan (getTransactions) where

import Control.Lens ((.~), (&), (^?))
import Data.Aeson (Value(..), (.:))
import Data.Aeson.Lens (key, _Array)
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Text (Text)
import Network.Wreq
import Data.Vector (toList)
import RedBanana.Types
import qualified Data.Text as T

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

getTransactions :: Text -> Int -> Int -> Text -> IO (Maybe [Transaction])
getTransactions address startBlock endBlock sort = do
    let query = defaults & param "module" .~ ["account"]
                         & param "action" .~ ["txlist"]
                         & param "address" .~ [address]
                         & param "startblock" .~ [T.pack $ show startBlock]
                         & param "endblock" .~ [T.pack $ show endBlock]
                         & param "sort" .~ [sort]
    r <- getWith query "http://api.etherscan.io/api"
    let tx = r ^? responseBody . key "result" . _Array
    return $ tx >>= mapM (parseMaybe toTransaction) . toList
