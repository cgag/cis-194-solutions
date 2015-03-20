module HW12Test
(hw12Tests)
where

import Test.Tasty
import Test.Tasty.Hspec

hw12Tests :: IO TestTree
hw12Tests = do
  hspecSuite <- testSpec "HW12" hspecTests
  return $ testGroup "tests" [hspecSuite]

hspecTests :: SpecWith ()
hspecTests =
  describe "HW12" $
    it "Ex 1" $
      shouldBe 'a' 'a'
