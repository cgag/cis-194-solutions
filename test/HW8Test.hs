module HW8Test
(hw8Tests)
where

import Test.Tasty
import Test.Tasty.Hspec

import HW8.Employee
import HW8.Party

hw8Tests :: IO TestTree
hw8Tests = do
  hspecSuite <- testSpec "HW8" hspecTests
  return $ testGroup "tests" [hspecSuite]

hspecTests :: SpecWith ()
hspecTests =
  describe "Ex 1" $ do
    describe "glCons" $
      it "adds and employee to GuestList and recomputes fun score" $
        shouldBe (glCons (Emp"Mitch" 10)
                         (GL [Emp "Louis" 12] 12))
                 (GL [Emp "Mitch" 10, Emp "Louis" 12] 22)
    describe "moreFun" $
      it "returns GuestList with more fun" $
        shouldBe (moreFun (GL [] 20)
                          (GL [] 30))
                 (GL [] 30)

    describe "treeFold" $ do
      it "Can sum the elements in a tree" $
        shouldBe (treeFold (+)
                           (0 :: Int)
                           (Node 10 [ Node 1 []
                                    , Node 2 [ Node 3 []
                                             ]
                                    ]))
                 16
