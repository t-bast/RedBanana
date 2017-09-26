module RedBanana.GraphGenerator (generate) where

import Data.Text (Text)    
import qualified Data.Text as T
import Formatting
import RedBanana.Types    

generate :: [Transaction] -> Text
generate txs =
    sformat ("digraph G {\n" % stext % "}") (foldr T.append T.empty edges)
    where
        formatTransactionEdge tx = sformat ("\t\"" % stext % "\" -> \"" % stext % "\" [label=\"" % int % "\"];\n") (from tx) (to tx) (value tx)
        edges = map formatTransactionEdge txs :: [Text]