import Test.Tasty
import Test.Tasty.HUnit

import HW3

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup "Unit Tests"
  [ testCase "HUnit works" $ do
        skips [] @?= ([] :: [[Int]])
        skips "ABCD"   @?= ["ABCD", "BD", "C", "D"]
        skips "hello!" @?= ["hello!", "el!", "l!", "l", "o", "!"]
        skips [1 :: Int] @?= [[1]]
        skips [True, False] @?= [[True, False], [False]]
  ]
