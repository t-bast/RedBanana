module RedBanana.EthereumClient where
    
import Control.Lens
import Data.Aeson (fromJSON, Result (..))
import Data.Aeson.Lens (key)
import Data.Text
import Network.Wreq
import RedBanana.Types

getTransactions :: Text -> IO (Maybe [Transaction])
getTransactions address = do
    let query = defaults & param "address" .~ [address]
    r <- getWith query "http://api.etherscan.io/api?module=account&action=txlist&startblock=0&endblock=99999999&sort=asc"
    let tx = r ^? responseBody . key "result"
    return $ case tx of
        Nothing -> Nothing
        Just v -> case fromJSON v of
            Error _ -> Nothing
            Success v -> Just v