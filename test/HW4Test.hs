module HW4Test
(hw4Tests)
where

import Test.Tasty
import Test.Tasty.Hspec

import HW4

hw4Tests :: IO TestTree
hw4Tests = do
  hspecSuite <- testSpec "HW4" hspecTests
  return $ testGroup "tests" [hspecSuite]

fun1' :: [Integer] -> Integer
fun1' [] = 1
fun1' (x:xs)
  | even x = (x - 2) * fun1' xs
  | otherwise = fun1' xs

-- fun2' :: Integer -> Integer
-- fun2' 1 = 0
-- fun2' n | even n = n + fun2' (n `div` 2)
--         | otherwise = fun2' (3 * n + 1)

hspecTests :: SpecWith ()
hspecTests = do
  describe "wholemeal programming" $
    describe "fun1" $
      it "it has the same results as fun1'" $ do
        fun1 [1..10] `shouldBe` fun1' [1..10]
        fun1 [] `shouldBe` fun1' []
        fun1 [1,-5,2] `shouldBe` fun1' [1,-5,2]
    -- describe "fun2" $
    --   it "it has the same results as fun2'" $ do
    --     map fun2 [-5..5] `shouldBe` map fun2' [-5..5]

  describe "Tree folding" $ do
    describe "height" $
      it "computes heights" $
        shouldBe
          (height (Node 3
                    (Node 2 Leaf
                            (2 :: Int)
                            (Node 1 Leaf 1 Leaf))
                    3
                    Leaf))
          3

    describe "insertBalanced" $ do
      runIO . print $ foldTree ""
      runIO . print $ foldTree "A"
      runIO . print $ foldTree "ABCD"
      runIO . print $ foldTree "ABCDE"
      it "builds balanced trees" $ do
        let tree = foldTree "A"
        height tree `shouldBe` 1
        let t2 = foldTree "AB"
        height t2 `shouldBe` 2
        let t3 = foldTree "ABC"
        height t3 `shouldBe` 2
        let t4 = foldTree "ACDE"
        height t4 `shouldBe` 3

    describe "xor" $
      it "returns True iff there are an odd number of True's in the list" $ do
        xor [False, True, False] `shouldBe` True
        xor [False, True, False, False, True] `shouldBe` False

    describe "map'" $
      it "behaves the same as map" $ do
        map' (+1) [1..5 :: Integer] `shouldBe` map (+1) [1..5]
        map' (++ "X") ["abc", "d"] `shouldBe` map (++ "X") ["abc", "d"]

    -- describe "Sieve of Sundaram" $
    --   it "generate all odd primes up to 2n+2 given n" $
    --     sieveSundaram 10 `shouldBe` [1,3,5,7]
