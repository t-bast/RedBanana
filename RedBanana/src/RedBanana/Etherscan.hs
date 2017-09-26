module RedBanana.Etherscan (getTransactions, getTransactionsFlow) where

import Control.Lens ((.~), (&), (^?))
import Data.Aeson (Value(..), (.:))
import Data.Aeson.Lens (key, _Array)
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (toList)
import Network.Wreq
import RedBanana.Types

toInt :: String -> Integer
toInt = read

toTransaction :: Value -> Parser Transaction
toTransaction (Object v) =
    Transaction
    <$> v .: "hash"
    <*> v .: "blockHash"
    <*> (toInt <$> v .: "blockNumber")
    <*> (toInt <$> v .: "confirmations")
    <*> v .: "from"
    <*> v .: "to"
    <*> (toInt <$> v .: "value")

getTransactions :: TransactionArgs -> IO (Maybe [Transaction])
getTransactions (TransactionArgs address startBlock endBlock sort) = do
    let query = defaults & param "module" .~ ["account"]
                         & param "action" .~ ["txlist"]
                         & param "address" .~ [address]
                         & param "startblock" .~ [T.pack $ show startBlock]
                         & param "endblock" .~ [T.pack $ show endBlock]
                         & param "sort" .~ [sort]
    r <- getWith query "http://api.etherscan.io/api"
    let tx = r ^? responseBody . key "result" . _Array
    return $ tx >>= mapM (parseMaybe toTransaction) . toList

getTransactionsFlow :: TransactionArgs -> Int -> IO [Transaction]
getTransactionsFlow _ 0 = return []
getTransactionsFlow args depth = do
    incomingTxs <- fmap (filter ((== address args) . to) . fromMaybe []) (getTransactions args)
    let sendersToAnalyze = filter (\tx -> from tx /= "GENESIS") incomingTxs
    let transactionArgs tx = TransactionArgs (from tx) (startBlock args) (endBlock args) (sort args)
    recursiveTxs <- mapM (\tx -> getTransactionsFlow (transactionArgs tx) (depth - 1)) sendersToAnalyze
    return $ concat $ incomingTxs : recursiveTxs