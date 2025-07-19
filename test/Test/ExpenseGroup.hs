{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.ExpenseGroup (tests) where

import Lib.ExpenseGroup (ExpenseEntry (..), ExpenseGroupSummary (..), expenseGroupSummary)
import Test.HUnit (Test, (~:), (~?=))

tests :: [Test]
tests =
  [ "expenseGroupSummary"
      ~: [ "empty group" ~: expenseGroupSummary [] ~?= ExpenseGroupSummary [],
           "non-empty group"
             ~: expenseGroupSummary
               [ ExpenseEntry {item = "a", price = 1},
                 ExpenseEntry {item = "a", price = 1}
               ]
             ~?= ExpenseGroupSummary [ExpenseEntry {item = "a", price = 2}]
         ]
  ]
