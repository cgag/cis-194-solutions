{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW7.Scrabble where

import Data.Monoid
import Data.Char (toLower)


newtype Score = Score Int deriving (Show, Eq, Ord, Num)

score :: Char -> Score
score c = score' (toLower c)
  where
    score' x
      | x `elem` ['a','e','i','l','n','o','r','s','t','u'] = 1
      | x `elem` ['d','g'] = 2
      | x `elem` ['b','p','c','m'] = 3
      | x `elem` ['v','h','y','f','w'] = 4
      | x `elem` ['k'] = 5
      | x `elem` ['j','x'] = 8
      | x `elem` ['q','z'] = 10
      | otherwise = 0

scoreString :: String -> Score
scoreString = sum . map score

instance Monoid Score where
  Score x `mappend` Score y = Score (x + y)
  mempty = Score 0
