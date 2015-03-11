module Main where

import HW3Test (hw3Tests)
import HW4Test (hw4Tests)
import HW5Test (hw5Tests)
import HW6Test (hw6Tests)

import Test.Tasty

-- TODO: eliminate all the duplication in these hwNTests fns
main :: IO ()
main = do
  testTrees <- sequence [hw3Tests, hw4Tests, hw5Tests, hw6Tests]
  defaultMain (testGroup "All Tests" testTrees)
