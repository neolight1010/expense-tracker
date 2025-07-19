module ExpensesCli.SummaryCommand (handleSummaryCommand) where

import Data.ByteString.Char8 (unpack)
import Data.Yaml (encode)
import Lib.Ledger (Ledger, expenseGroupSummary)

handleSummaryCommand :: Ledger -> String -> IO ()
handleSummaryCommand ledger = putStrLn . unpack . encode . expenseGroupSummary ledger
