import Lib (Expenses (..), Item (..), expensesTotal)
import Test.HUnit (Test (TestCase, TestLabel, TestList), runTestTT, (@?=), (~?=))

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestLabel "lib tests" $
        TestList
          [ expensesTotal (Expenses [])
              ~?= 0,
            expensesTotal
              (Expenses [(Item "item", 1)])
              ~?= 1
          ]

  pure ()
