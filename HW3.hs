module HW3 where

import qualified Data.Map.Strict as M
import Data.List

skips :: [a] -> [[a]]
skips [] = []
skips xs = map (`takeNths` xs) [1..length xs]


takeNths :: Int -> [a] -> [a]
takeNths _ [] = []
takeNths n xs = let rest = drop (n - 1) xs
                in take 1 rest ++ takeNths n (drop 1 rest)

localMaxima :: [Integer] -> [Integer]
localMaxima []    = []
localMaxima [_]   = []
localMaxima [_,_] = []
localMaxima (x:y:z:xs)
  | y > x && y > z = y : localMaxima (y:z:xs)
  | otherwise = localMaxima (y:z:xs)

histogram :: [Integer] -> String
histogram [] = intercalate "\n" ["==========", "0123456789"]
histogram ints = intercalate "\n" $ render freqMap ++ ["==========", "0123456789"]
  where
    freqMap = freqs ints
    render m = go (maximum . map snd $ M.toList m)
    go 0 = []
    go n = concatMap
            (\k -> if M.findWithDefault 0 k freqMap >= n then "*" else " ")
            [0..9]
           : go (n - 1)

freqs :: (Eq a, Ord a) => [a] -> M.Map a Int
freqs = foldl' buildMap M.empty
  where
    buildMap m k = M.insertWith (+) k 1 m
