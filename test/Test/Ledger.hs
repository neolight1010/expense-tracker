{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.Ledger (tests) where

import Data.Either (isLeft)
import Data.Map (fromList)
import Data.Yaml.Internal (ParseException)
import qualified Lib.ExpenseGroup as ExpenseGroup
import Lib.Ledger (Ledger (..), decodeLedger, expenseGroup, expenseGroupNames, expenseGroupSummary)
import Test.HUnit (Test (TestLabel, TestList), (~:), (~?), (~?=))
import Lib.Item (Item(..))

tests :: [Test]
tests =
  [ TestLabel
      "ledger io"
      $ TestList
        [ let decoded :: Either ParseException Ledger
              decoded = decodeLedger ""
           in isLeft decoded ~? "expected decoding to fail",
          let decoded :: Either ParseException Ledger
              decoded =
                decodeLedger
                  "expenses:\n\
                  \  group1:\n\
                  \    - item: item1\n\
                  \      logs:\n\
                  \        - 0.1\n\
                  \\n\
                  \items:\n\
                  \  item1:\n\
                  \    category: category1\n\
                  \"
           in case decoded of
                Left e -> False ~? "decoding should not have failed: " ++ show e
                Right ledger ->
                  ledger
                    ~?= Ledger
                      ( fromList
                          [ ( "group1",
                              [ ExpenseGroup.ExpenseEntry {ExpenseGroup.item = "item1", ExpenseGroup.logs = [0.1]}
                              ]
                            )
                          ]
                      )
                      ( fromList
                        [ ("item1", Item { category = "category1" }) ] )
        ],
    "expenseGroupNames"
      ~: [ "empty ledger" ~: expenseGroupNames (Ledger mempty mempty) ~?= [],
           "non-empty ledger"
             ~: expenseGroupNames
               ( Ledger
                   ( fromList
                       [ ("group1", []),
                         ("group2", [])
                       ]
                   )
                   mempty
               )
             ~?= ["group1", "group2"]
         ],
    "expenseGroup"
      ~: [ "not-found group" ~: expenseGroup (Ledger mempty mempty) "group" ~?= Nothing,
           "found group" ~: expenseGroup (Ledger (fromList [("group1", [])]) mempty) "group1" ~?= Just []
         ],
    "expenseGroupSummary"
      ~: [ "not-found group" ~: expenseGroupSummary (Ledger mempty mempty) "group" ~?= Nothing,
           "found group"
             ~: expenseGroupSummary
               ( Ledger
                   ( fromList
                       [("group1", [])]
                   )
                   mempty
               )
               "group1"
             ~?= Just (ExpenseGroup.expenseGroupSummary mempty [])
         ]
  ]
