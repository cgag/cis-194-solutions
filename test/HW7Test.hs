{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HW7Test
(hw7Tests)
where

import Test.Tasty
import Test.Tasty.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (modifyMaxSuccess)

import Control.Applicative
import Data.List

import HW7.HW7
import HW7.Sized
import HW7.Scrabble


instance Arbitrary Size where
  arbitrary = fmap Size (arbitrary `suchThat` (> 0))

instance (Arbitrary a) => Arbitrary (JoinList Size a) where
  arbitrary =
    oneof [ do
                j1 <- arbitrary
                j2 <- arbitrary
                return $ Append (tag j1 + tag j2)  j1  j2
          , Single <$> pure 1 <*> arbitrary
          , return Empty
          ]

hw7Tests :: IO TestTree
hw7Tests = do
  hspecSuite <- testSpec "HW7" hspecTests
  return $ testGroup "tests" [hspecSuite]

hspecTests :: SpecWith ()
hspecTests =
  modifyMaxSuccess (+900) $ do
    describe "indexJ" $
        it "satisfies: (indexJ i jl) == (jlToList jl !!? i)" $
          property $ \i (jl :: JoinList Size String) ->
            indexJ i jl == (jlToList jl !!? i)

    describe "dropJ" $
      it "satifies: jlToList (dropJ n jl) == drop n (jlToList jl)" $
        property $ \n (jl ::JoinList Size String) ->
          jlToList (dropJ n jl) == drop n (jlToList jl)

    describe "takeJ" $
      it "satisfies: jlToList (takeJ n jl) == take n (jlToList jl)" $
        property $ \n (jl :: JoinList Size String) ->
          jlToList (takeJ n jl) == take n (jlToList jl)

    describe "scoreLine" $
      it "scores lines using scrabble rules" $
        shouldBe
          (scoreLine "yay " +++ scoreLine "haskell!")
          (Append (Score 23)
                  (Single (Score 9)  "yay ")
                  (Single (Score 14) "haskell!"))

    describe "mergeSort" $
      it "mergeSort xs == sort xs" $
        property $ \(xs :: [Int]) ->
          mergeSort xs == sort xs
