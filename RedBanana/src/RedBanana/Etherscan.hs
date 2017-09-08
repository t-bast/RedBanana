module RedBanana.Etherscan (getTransactions) where
    
import Control.Lens ((^.), (.~), (&), (^?))    
import Data.Aeson (Value)
import Data.Aeson.Lens (key, _Integer, _String, _Array)
import Data.Text (Text)
import Network.Wreq
import Data.Vector (toList)
import RedBanana.Types

toTransaction :: Value -> Maybe Transaction
toTransaction val = Transaction 
    <$> val ^? key "blockHash" . _String
    <*> val ^? key "blockNumber" . _Integer
    <*> val ^? key "confirmations" . _Integer
    <*> val ^? key "from" . _String
    <*> val ^? key "to" . _String
    <*> val ^? key "value" . _Integer

getTransactions :: Text -> IO (Maybe [Transaction])
getTransactions address = do
    let query = defaults & param "address" .~ [address]
    r <- getWith query "http://api.etherscan.io/api?module=account&action=txlist&startblock=0&endblock=10&sort=asc"
    let tx = r ^? responseBody . key "result" . _Array
    return $ case tx of 
        Nothing -> Nothing
        Just xs -> mapM toTransaction $ toList xs