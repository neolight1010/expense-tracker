{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.Ledger (tests) where

import Data.Either (isLeft)
import Data.Map (fromList)
import Data.Yaml.Internal (ParseException)
import qualified Lib.ExpenseGroup as ExpenseGroup
import Lib.Ledger (Ledger (..), decodeLedger, expenseGroup, expenseGroupNames, expenseGroupSummary)
import Test.HUnit (Test (TestLabel, TestList), (~:), (~?), (~?=))

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
                  \      price: 0.1\n\
                  \"
           in case decoded of
                Left e -> False ~? "decoding should not have failed: " ++ show e
                Right ledger ->
                  ledger
                    ~?= Ledger
                      ( fromList
                          [ ( "group1",
                              [ ExpenseGroup.ExpenseEntry {ExpenseGroup.item = "item1", ExpenseGroup.price = 0.1}
                              ]
                            )
                          ]
                      )
        ],
    "expenseGroupNames"
      ~: [ "empty ledger" ~: expenseGroupNames (Ledger mempty) ~?= [],
           "non-empty ledger"
             ~: expenseGroupNames
               ( Ledger
                   ( fromList
                        [ ("group1", []),
                         ("group2", [])
                       ]
                   )
               )
             ~?= ["group1", "group2"]
         ],
    "expenseGroup"
      ~: [ "not-found group" ~: expenseGroup (Ledger mempty) "group" ~?= Nothing,
           "found group" ~: expenseGroup (Ledger (fromList [("group1", [])])) "group1" ~?= Just []
         ],
    "expenseGroupSummary"
      ~: [ "not-found group" ~: expenseGroupSummary (Ledger mempty) "group" ~?= Nothing,
           "found group"
             ~: expenseGroupSummary
               ( Ledger
                   ( fromList
                       [("group1", [])]
                   )
               )
               "group1"
             ~?= Just (ExpenseGroup.expenseGroupSummary [])
         ]
  ]
