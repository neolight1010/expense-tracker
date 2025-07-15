module ListCommand (handleListCommand) where

import Lib (Ledger, expenseGroupNames)

handleListCommand :: Ledger -> IO ()
handleListCommand ledger =
  let groupLines = (unlines . expenseGroupNames) ledger
   in putStrLn groupLines
