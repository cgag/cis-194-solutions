{-# OPTIONS_GHC -fno-warn-orphans #-}

module HW8.Party where

import Data.Monoid

import HW8.Employee

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ eFun) (GL es glFun) = GL (e:es) (eFun + glFun)

instance Monoid GuestList where
  mempty  = GL [] 0
  mappend (GL xs f1) (GL ys f2) = GL (xs ++ ys) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1 g2
  | fun g1 >= fun g2 = g1
  | otherwise        = g2
  where
    fun (GL _ f) = f

data Tree a = Node
    { rootLabel :: a
    , subForest :: [Tree a]
    }

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f z (Node a []) = f z a
treeFold f z (Node a as) = f (treeFold' as) a
  where
    treeFold' = foldl (treeFold f) z

-- got stuck here, thanks github
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e xs = (bestWith, bestWithout)
  where bestWith    = glCons e . mconcat $ map snd xs
        bestWithout = mconcat . map (uncurry moreFun) $ xs

maxFun :: Tree Employee -> GuestList
maxFun = undefined
