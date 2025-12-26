{-# LANGUAGE OverloadedStrings #-}

module ExpensesCli.SummaryCommand (handleSummaryCommand) where

import Data.ByteString.Char8 (unpack)
import Data.Function ((&))
import Data.List (intersperse)
import Data.String (fromString)
import Data.Yaml (encode)
import Lib.ExpenseGroup (ExpenseGroupSummary (categoryTotals, itemTotals, total))
import Lib.Ledger (Ledger, expenseGroupSummary)
import qualified Rainbow

handleSummaryCommand :: Ledger -> String -> IO ()
handleSummaryCommand ledger = Rainbow.putChunksLn . showSummary . expenseGroupSummary ledger

showSummary :: Maybe ExpenseGroupSummary -> [Rainbow.Chunk]
showSummary summary =
  [ [Rainbow.fore Rainbow.red "Expense Group - Summary\n"],
    showCategoryTotals summary,
    showItemTotals summary,
    showGrandTotal summary
  ]
    & (intersperse "\n" . concat)

showCategoryTotals :: Maybe ExpenseGroupSummary -> [Rainbow.Chunk]
showCategoryTotals summary =
  let categories :: [Rainbow.Chunk]
      categories =
        (\(category, total_) -> Rainbow.bold (fromString (category ++ ": ")) <>  fromString (show total_) <> "\n")
          <$> concatMap categoryTotals summary
   in [ Rainbow.fore Rainbow.cyan "Category totals:",
        Rainbow.fore Rainbow.white (mconcat categories)
      ]

showItemTotals :: Maybe ExpenseGroupSummary -> [Rainbow.Chunk]
showItemTotals summary =
  [ Rainbow.fore Rainbow.cyan "Item totals:",
    Rainbow.fore Rainbow.white (fromString $ unpack $ encode (itemTotals <$> summary))
  ]

showGrandTotal :: Maybe ExpenseGroupSummary -> [Rainbow.Chunk]
showGrandTotal summary =
  [ Rainbow.fore Rainbow.cyan "Grand total:",
    Rainbow.fore Rainbow.white (fromString $ unpack $ encode (total <$> summary))
  ]
