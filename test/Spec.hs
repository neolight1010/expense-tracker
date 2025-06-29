import Test.HUnit (Test (TestCase, TestLabel, TestList), runTestTT, (@?=))
import Lib (helloWorld)

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestLabel "lib tests" $
        TestList
          [ TestCase $
              helloWorld @?= "Hello, World!"
          ]

  pure ()
