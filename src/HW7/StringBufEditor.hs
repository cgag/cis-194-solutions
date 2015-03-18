module HW7.StringBufEditor where

import HW7.HW7
import HW7.Buffer
import HW7.Sized
import HW7.Scrabble
import HW7.Editor

main :: IO ()
main = runEditor editor $ ((foldr ((+++) . fromString) Empty
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]) :: JoinList (Score, Size) String)
