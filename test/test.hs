import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec

import qualified  Data.Map.Strict as M

import HW3


main :: IO ()
main = do
  testTree <- tests
  defaultMain testTree

tests :: IO TestTree
tests = do
  hspecSuite <- testSpec "HSpec Unit Tests" hspecTests
  return $ testGroup "Tests" [unitTests, hspecSuite]

hspecTests :: SpecWith ()
hspecTests = do
  describe "Local maximum (Problem 2)" $
    it "finds local maxima" $ do
      shouldBe (localMaxima [2,9,5,6,1]) [9,6]
      shouldBe (localMaxima [2,3,4,1,5]) [4]
      shouldBe (localMaxima [1,2,3,4,5]) []
  describe "Histogram" $ do
    describe "freqs" $
      it "builds a frequency map" $
        shouldBe (freqs [1 :: Integer,2,3,1]) (M.fromList [(1,2), (2, 1), (3, 1)])
    describe "rendering" $ do
        -- TODO: assertions instead of visual inspection
        runIO $ putStrLn (histogram [1,1,1,5])
        runIO $ putStrLn (histogram [1,4,5,4,6,6,3,4,2,4,9])


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
