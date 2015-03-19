module HW10Test
(hw10Tests)
where

import Test.Tasty
import Test.Tasty.Hspec

import Control.Applicative
import HW10.AParser

hw10Tests :: IO TestTree
hw10Tests = do
  hspecSuite <- testSpec "HW10" hspecTests
  return $ testGroup "tests" [hspecSuite]

data T = T Integer Char deriving (Show, Eq)

hspecTests :: SpecWith ()
hspecTests =
  describe "Parser" $ do
    describe "Functor instance" $
      it "applies function in a parser context" $
        shouldBe (runParser (fmap (+1) posInt) "1")
                 (Just (2, ""))

    describe "Applicative instance" $
      it "Works?" $ do
        shouldBe (runParser (T <$> posInt <*> char 'a') "1a")
                 (Just (T 1 'a', ""))

    describe "abParser" $
      it "returns (Just (('a','b'), \"cdef\")" $
        shouldBe (runParser abParser "abcdef")
                 (Just (('a','b'), "cdef"))

    describe "abParser_" $
      it "returns Just ((), \"cdef\")" $
        shouldBe (runParser abParser_ "abcdef")
                 (Just ((), "cdef"))

    describe "intPair" $
      it "parses 2 space-separated ints into a list" $
        shouldBe (runParser intPair "12 34")
                 (Just ([12,34], ""))

    describe "intOrUppercase" $
      it "consumes ints or uppercase chars" $ do
        shouldBe (runParser intOrUppercase "234abcd")
                 (Just ((), "abcd"))

        shouldBe (runParser intOrUppercase "XYZ")
                 (Just ((), "YZ"))

        shouldBe (runParser intOrUppercase "foo")
                 Nothing
