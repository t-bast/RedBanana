module RedBanana.EthereumClient where
    
import Control.Lens
import Data.Aeson (Value)
import Data.Aeson.Lens (key)
-- import Data.ByteString
import Data.Text
import Network.Wreq
import RedBanana.Types

-- getTransactions :: Text -> IO (Response [Transaction])
-- getTransactions address = do
--     let query = defaults & param "address" .~ [address]
--     r <- getWith query "http://api.etherscan.io/api?module=account&action=txlist&startblock=0&endblock=99999999&sort=asc"
--     asJSON =<< r ^? responseBody . key "result"
    -- r ^? responseBody . key "result"
    -- return (fromJSON (r ^. responseBody . key "result"))
    -- asJSON =<< r ^. responseBody . key "result"