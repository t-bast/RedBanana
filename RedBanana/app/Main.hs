module Main where

import RedBanana.Etherscan
import RedBanana.GraphGenerator

import Control.Monad
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Options.Applicative

data Args = Args
    { address      :: String
    , blockChain   :: String
    , depth        :: Int
    , startBlock   :: Int
    , endBlock     :: Int
    , verbose      :: Bool }

args :: Parser Args
args = Args
        <$> argument str
            ( metavar "address"
            <> help "Address to analyze" )
        <*> strOption
            ( long "blockchain"
            <> short 'b'
            <> metavar "string"
            <> help "Blockchain to analyze"
            <> showDefault
            <> value "ethereum")
        <*> option auto
            ( long "depth"
            <> short 'd'
            <> metavar "int"
            <> help "Depth of the transactions graph"
            <> showDefault
            <> value 1)
        <*> option auto
            ( long "startBlock"
            <> short 's'
            <> metavar "int"
            <> help "Start block"
            <> showDefault
            <> value 0)
        <*> option auto
            ( long "endBlock"
            <> short 'e'
            <> metavar "int"
            <> help "End block"
            <> showDefault
            <> value 99999999)
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
            <> progDesc "Analyze transaction flow made to a blockchain address"
            <> header "Red Banana" )

analyze :: Args -> IO ()
analyze (Args addr "ethereum" depth startBlock endBlock verbose) = do
    when verbose $ putStrLn $ "Searching for transactions sent to " ++ addr
    when verbose $ putStrLn $ "Maximum graph depth: " ++ show depth
    tx <- getTransactionsFlow (T.pack addr) startBlock endBlock "asc" depth
    when verbose $ putStrLn $ (show . length $ tx) ++ " transactions found"
    putStr $ T.unpack . generate $ tx
analyze (Args _ blockchain _ _ _ _) = 
    putStrLn $ "Analysis of " ++ blockchain ++ " blockchain is not supported yet. Try RedBanana-exe --help for help."