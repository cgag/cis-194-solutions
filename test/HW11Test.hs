module HW11Test
  (hw11Tests)
  where

import Test.Tasty
import Test.Tasty.Hspec

import Data.Char
import Data.Maybe

import HW11.AParser
import HW11.SExpr

hw11Tests :: IO TestTree
hw11Tests = do
  hspecSuite <- testSpec "HW11" hspecTests
  return $ testGroup "tests" [hspecSuite]

hspecTests :: SpecWith ()
hspecTests = do
  describe "Ex 1" $ do
    describe "zeroOrMore" $ do
      it "parses as many as values as possible" $
        shouldBe (runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH")
                 (Just ("ABC", "dEfgH"))
      it "always succeeds" $
        shouldBe (runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh")
                 (Just ("", "abcdeFGh"))

    describe "oneOrMore" $ do
      it "fails if there isn't at least one result" $
        shouldBe (runParser (oneOrMore (satisfy isUpper)) "abcdEfgH")
                 Nothing

      it "works like zeroOrMore otherwise" $
        shouldBe (runParser (oneOrMore  (satisfy isUpper)) "ABCdEfgH")
                 (runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH")

  describe "Ex 2" $ do
    describe "spaces" $
      it "parses zero or more whitespace chars" $ do
        shouldBe (runParser spaces "  a") (Just ("  ", "a"))
        shouldBe (runParser spaces "a")   (Just ("", "a"))

    describe "ident" $
      it "parses an alpha char followed  by 0 or more alphaNums" $ do
        shouldBe (runParser ident "foobar baz") (Just ("foobar", " baz"))
        shouldBe (runParser ident "foo33fa")    (Just ("foo33fa", ""))
        shouldBe (runParser ident "2bad")       Nothing
        shouldBe (runParser ident "")           Nothing

  describe "Ex 3" $ do
    describe "parsing parseSExpr" $ do
      it "parses integers" $
        shouldBe (fst . fromJust $ runParser parseSExpr "5")
                 (A (N 5))
      it "parses identifiers" $
        shouldBe (fst . fromJust $ runParser parseSExpr "foo3")
                 (A (I "foo3"))

      it "leading and trailing whitespace" $
        shouldBe (fst . fromJust $ runParser parseSExpr "   4   ")
                 (A (N 4))

      it "parses lists of sexprs" $ do
        shouldBe (fst . fromJust $ runParser parseSExpr "(bar (foo) 3 5 874)")
                 (Comb [ A (I "bar")
                       , Comb [A (I "foo")]
                       , A (N 3)
                       , A (N 5)
                       , A (N 874)
                       ])

        shouldBe (fst . fromJust $ runParser parseSExpr
                            "(((lambda x  (lambda y (plus x y))) 3) 5)")
                 (Comb [ Comb [ Comb [ A (I "lambda")
                                     , A (I "x")
                                     , Comb [ A (I "lambda")
                                            , A (I "y")
                                            , Comb [ A (I "plus")
                                                   , A (I "x")
                                                   , A (I "y")
                                                   ]
                                            ]
                                      ]
                              , A (N 3)
                              ]
                        , A (N 5)
                        ])

        shouldBe (fst . fromJust $ runParser parseSExpr
                    "(   lots of   (  spaces  in )  this (one ) )")
                 (Comb [ A (I "lots")
                       , A (I "of")
                       , Comb [ A (I "spaces")
                              , A (I "in")
                              ]
                       , A (I "this")
                       , Comb [ A (I "one") ]
                       ])
