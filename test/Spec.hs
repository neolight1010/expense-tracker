{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Data.Either (isLeft)
import Data.Map (fromList)
import Data.Yaml.Internal (ParseException)
import Lib.ExpenseGroup (ExpenseEntry (..), ExpenseGroupSummary (..), expenseGroupSummary)
import Lib.Ledger
  ( Ledger (..),
    decodeLedger,
    expenseGroupNames,
  )
import Test.HUnit (Test (TestLabel, TestList), runTestTT, (~:), (~?), (~?=))

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestLabel "lib tests" $
        TestList
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
                                      [ ExpenseEntry {item = "item1", price = 0.1}
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
            "expenseGroupSummary"
              ~: [ "empty group" ~: expenseGroupSummary [] ~?= ExpenseGroupSummary [],
                   "non-empty group"
                     ~: expenseGroupSummary
                       [ ExpenseEntry {item = "a", price = 1},
                         ExpenseEntry {item = "a", price = 1}
                       ]
                     ~?= ExpenseGroupSummary [ExpenseEntry {item = "a", price = 2}]
                 ]
          ]

  pure ()
