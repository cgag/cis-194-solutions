module HW6Test
(hw6Tests)
where

import Test.Tasty
import Test.Tasty.Hspec

import HW6

hw6Tests :: IO TestTree
hw6Tests = do
  hspecSuite <- testSpec "HW6" hspecTests
  return $ testGroup "tests" [hspecSuite]

hspecTests :: SpecWith ()
hspecTests = do
  describe "streams" $
    describe "Ex4" $ do
      describe "Repeat" $ do
        it "works like list repeat" $
          show (streamRepeat "x") `shouldBe` show (take 20 (repeat "x"))
      describe "Map" $ do
        it "works like list map" $
          shouldBe
            (show (streamMap (++"b") (streamRepeat "a")))
            (show (take 20 . map (++"b") . repeat $ "a"))
      describe "streamFromSeed" $
        it "works like iterate" $
          shouldBe
            (show (streamFromSeed (++"b") "a"))
            (show (take 20 $ iterate (++"b") "a"))
    -- describe "Ex5" $

