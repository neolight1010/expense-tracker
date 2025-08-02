{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Test.HUnit (runTestTT, (~:))
import qualified Test.Ledger
import qualified Test.ExpenseGroup

main :: IO ()
main = do
  _ <-
    runTestTT $
      "lib tests" ~: [
      "ledger tests" ~: Test.Ledger.tests,
      "expense group summary tests" ~: Test.ExpenseGroup.tests
      ]

  pure ()
