{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Test.HUnit (runTestTT, (~:))
import qualified Test.Ledger

main :: IO ()
main = do
  _ <-
    runTestTT $
      "lib tests" ~: Test.Ledger.tests

  pure ()
