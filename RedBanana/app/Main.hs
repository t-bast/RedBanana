module Main where

import RedBanana.Etherscan
import RedBanana.GraphGenerator

import qualified Data.Text as T
import Options.Applicative
import Data.Semigroup ((<>))

data Args = Args
    { address      :: String
    , verbose      :: Bool }

args :: Parser Args
args = Args
        <$> argument str
            ( metavar "address"
            <> help "Address to analyze" )
        <*> switch
            ( long "verbose"
            <> short 'v'
            <> help "Output verbosity" )

main :: IO ()
main =
    analyze =<< execParser opts
        where
        opts = info (args <**> helper)
            ( fullDesc
            <> progDesc "Analyze transaction to an ethereum address"
            <> header "Red Banana" )

analyze :: Args -> IO ()
analyze (Args addr False) = do
    let depth = 2
    putStrLn $ "Searching for transactions sent to " ++ addr
    putStrLn $ "Maximum graph depth: " ++ show depth
    tx <- getTransactionsFlow (T.pack addr) 0 99999999 "asc" depth
    putStrLn $ (show . length $ tx) ++ " transactions found"
    putStr $ T.unpack . generate $ tx
analyze _ = return ()
