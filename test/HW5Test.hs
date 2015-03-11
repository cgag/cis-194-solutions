module HW5Test
(hw5Tests)
where

import Test.Tasty
import Test.Tasty.Hspec

import HW5.ExprT
import qualified HW5.StackVM as VM

import HW5.HW5

hw5Tests :: IO TestTree
hw5Tests = do
  hspecSuite <- testSpec "HW5" hspecTests
  return $ testGroup "tests" [hspecSuite]

hspecTests :: SpecWith ()
hspecTests = do
  describe "Ex1: eval" $
    it "evals ExprT expressions" $
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

  describe  "Ex2: evalStr" $
    it "evalutes strings" $ do
      evalStr "(2+3)*4" `shouldBe` Just 20
      evalStr "(23)5" `shouldBe`  Nothing

  describe "Ex5: StackVM" $
    it "convert expr into stackvm programs" $ do
      shouldBe (compile "(2+3)*4")
               (Just [ VM.PushI 2
                     , VM.PushI 3
                     , VM.Add
                     , VM.PushI 4
                     , VM.Mul
                     ])
