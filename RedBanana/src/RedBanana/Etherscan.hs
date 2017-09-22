module RedBanana.Etherscan (getTransactions, generateGraph) where

import Control.Lens ((.~), (&), (^?))
import Data.Aeson (Value(..), (.:))
import Data.Aeson.Lens (key, _Array)
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Text (Text)
import Network.Wreq
import Data.Vector (toList)
import RedBanana.Types
import qualified Data.Text as T
import Formatting
import Formatting.Formatters

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


generateGraph :: [Transaction] -> Text
generateGraph txs =
    let edges = map (\tx -> sformat ("\"" % stext % "\" -> \"" % stext % "\" [label=\"" % int % "\"];") (from tx) (to tx) (value tx)) txs :: [Text]
    in sformat ("digraph G { " % stext % " }") (foldr T.append T.empty edges)

getTransactionsFlow :: Text -> Int -> IO [Transaction]
getTransactionsFlow addr 0 = pure $ Just []
getTransactionsFlow addr n = do
    inTxs <- getTransactions addr 0 9999999 "asc" >>= return . filter ((== addr) . to) . maybe [] id 