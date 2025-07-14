{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Data.Either (isLeft)
import Data.Map (fromList)
import Data.Yaml (decodeEither')
import Data.Yaml.Internal (ParseException)
import Lib (ExpenseEntry (..), Ledger (..))
import Test.HUnit (Test (TestLabel, TestList), runTestTT, (~?), (~?=))

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
                      decoded = decodeEither' ""
                   in isLeft decoded ~? "expected decoding to fail",
                  let decoded :: Either ParseException Ledger
                      decoded =
                        decodeEither'
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
                ]
          ]

  pure ()
