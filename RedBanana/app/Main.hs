module Main where

import RedBanana.Etherscan
import RedBanana.Types

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
    tx <- getTransactions $ T.pack addr
    print $ case tx of
        Nothing -> "Invalid address"
        Just tx -> show $ head tx
analyze _ = return ()