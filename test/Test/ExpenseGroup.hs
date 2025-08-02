{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.ExpenseGroup (tests) where

import qualified Data.Map as Map
import Lib.ExpenseGroup (ExpenseEntry (..), ExpenseGroupSummary (..), expenseGroupSummary)
import Lib.Item (Item (..))
import Test.HUnit (Test, (~:), (~?=))

tests :: [Test]
tests =
  [ "expenseGroupSummary"
      ~: [ "empty group" ~: expenseGroupSummary mempty [] ~?= ExpenseGroupSummary [] [],
           "non-empty group"
             ~: expenseGroupSummary
               ( Map.fromList
                   [("a", Item {category = "category1"})]
               )
               [ ExpenseEntry {item = "a", price = 1},
                 ExpenseEntry {item = "a", price = 1},
                 ExpenseEntry {item = "b", price = 1}
               ]
             ~?= ExpenseGroupSummary
               [ ExpenseEntry {item = "a", price = 2},
                 ExpenseEntry {item = "b", price = 1}
               ]
               [("[no-category]", 1), ("category1", 2)]
         ]
  ]
