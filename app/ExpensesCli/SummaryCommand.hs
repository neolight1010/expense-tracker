{-# LANGUAGE OverloadedStrings #-}

module ExpensesCli.SummaryCommand (handleSummaryCommand) where

import Data.ByteString.Char8 (unpack)
import Data.Function ((&))
import Data.String (fromString)
import Data.Yaml (encode)
import Lib.ExpenseGroup (ExpenseGroupSummary (categoryTotals, itemTotals, total))
import Lib.Ledger (Ledger, expenseGroupSummary)
import qualified Rainbow

handleSummaryCommand :: Ledger -> String -> IO ()
handleSummaryCommand ledger = Rainbow.putChunksLn . showSummary . expenseGroupSummary ledger

showSummary :: Maybe ExpenseGroupSummary -> [Rainbow.Chunk]
showSummary summary =
  [ [Rainbow.fore Rainbow.red "Expense Group - Summary", "\n\n"],
    showCategoryTotals summary,
    ["\n"],
    showItemTotals summary,
    ["\n"],
    showGrandTotal summary
  ]
    & concat

showCategoryTotals :: Maybe ExpenseGroupSummary -> [Rainbow.Chunk]
showCategoryTotals summary =
  let categories :: [Rainbow.Chunk]
      categories = concatMap showTuple (concatMap categoryTotals summary)
   in Rainbow.fore Rainbow.cyan "Category totals:\n" : categories 

showItemTotals :: Maybe ExpenseGroupSummary -> [Rainbow.Chunk]
showItemTotals summary =
  let items :: [Rainbow.Chunk]
      items = concatMap showTuple (concatMap itemTotals summary)
   in Rainbow.fore Rainbow.cyan "Item totals:\n" : items
      

showGrandTotal :: Maybe ExpenseGroupSummary -> [Rainbow.Chunk]
showGrandTotal summary =
  [ Rainbow.fore Rainbow.cyan "Grand total: ",
    Rainbow.fore Rainbow.white (fromString $ unpack $ encode (total <$> summary))
  ]

showTuple :: (String, Float) -> [Rainbow.Chunk]
showTuple (title, value) = [Rainbow.bold (fromString (title ++ ": ")), fromString (show value) <> "\n"]
