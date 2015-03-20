module Main where

import           HW3Test    (hw3Tests)
import           HW4Test    (hw4Tests)
import           HW5Test    (hw5Tests)
import           HW6Test    (hw6Tests)
import           HW7Test    (hw7Tests)
import           HW8Test    (hw8Tests)
import           HW10Test   (hw10Tests)
import           HW11Test   (hw11Tests)
import           HW12Test   (hw12Tests)

import           Test.Tasty

-- TODO: eliminate all the duplication in these hwNTests fns
main :: IO ()
main = do
  testTrees <- sequence [ hw3Tests
                        , hw4Tests
                        , hw5Tests
                        , hw6Tests
                        , hw7Tests
                        , hw8Tests
                        , hw10Tests
                        , hw11Tests
                        , hw12Tests
                        ]
  defaultMain (testGroup "All Tests" testTrees)
