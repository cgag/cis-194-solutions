{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HW7.HW7 where

import Data.Monoid

import HW7.Sized
import HW7.Scrabble
import HW7.Buffer


data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ j1 j2) = jlToList j1 ++ jlToList j2

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i - 1)

tagSize :: (Sized b, Monoid b) => JoinList b a -> Int
tagSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ i _ | i < 0    = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append _ j1 j2)
  | i  >= tagSize j1 = indexJ (i - tagSize j1) j2
  | otherwise        = indexJ i j1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty         = Empty
dropJ i jl |i <= 0    = jl
dropJ _ (Single _ _)  = Empty
dropJ i (Append _ left right)
  | i < tagSize left = dropJ i left +++ right
  | otherwise = dropJ (i - tagSize left) right

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty      = Empty
takeJ i _ | i <= 0 = Empty
takeJ _ jl@(Single _ _) = jl
takeJ i (Append _ left right)
  | i > tagSize left = left +++ takeJ (i - tagSize left) right
  | otherwise = takeJ i left

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
  toString = concat . jlToList
  fromString s = let fromString' s' = Single (scoreString s', 1) s'
                 in foldr ((+++) . fromString') Empty (lines s)
  line = indexJ
  replaceLine i s jl = case indexJ i jl of
                         Nothing -> jl
                         Just _  ->
                           takeJ (i - 1) jl +++ fromString s +++ dropJ i jl

  numLines = getSize . snd . tag
  value jl = let (Score s, _) = tag jl in s
