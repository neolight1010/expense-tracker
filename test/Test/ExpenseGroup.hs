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
      ~: [ "empty group" ~: expenseGroupSummary mempty [] ~?= ExpenseGroupSummary [] [] 0,
           "non-empty group"
             ~: expenseGroupSummary
               ( Map.fromList
                   [("a", Item {category = "category1"})]
               )
               [ ExpenseEntry {item = "a", logs = [1, 1]},
                 ExpenseEntry {item = "a", logs = [1]},
                 ExpenseEntry {item = "b", logs = [1, 1]}
               ]
             ~?= ExpenseGroupSummary
               [ ("a", 3),
                 ("b", 2)
               ]
               [("[no-category]", 2), ("category1", 3)]
               5
         ]
  ]
