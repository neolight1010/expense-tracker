module ExpensesCli.SummaryCommand (handleSummaryCommand) where

import Data.ByteString.Char8 (unpack)
import Data.Yaml (encode)
import Lib.Ledger (Ledger, expenseGroupSummary)
import Lib.ExpenseGroup (ExpenseGroupSummary)
import qualified Rainbow
import Data.String (fromString)

handleSummaryCommand :: Ledger -> String -> IO ()
handleSummaryCommand ledger = Rainbow.putChunkLn . showSummary . expenseGroupSummary ledger

showSummary :: Maybe ExpenseGroupSummary -> Rainbow.Chunk
showSummary summary = Rainbow.fore Rainbow.blue (fromString $ unpack $ encode summary)
