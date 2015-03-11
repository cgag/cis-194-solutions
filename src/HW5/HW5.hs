{-# LANGUAGE TypeSynonymInstances #-}

module HW5.HW5 where

import Control.Monad

import HW5.ExprT
import HW5.Parser

import qualified HW5.StackVM as VM

class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  mul :: e -> e -> e

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (mul a b)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 a) (Mod7 b) = Mod7 (a + b `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 (a * b `mod` 7)

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Mul expr1 expr2) = eval expr1 * eval expr2
eval (Add expr1 expr2) = eval expr1 + eval expr2

evalStr :: String -> Maybe Integer
evalStr s = liftM eval (parseExp Lit Add Mul s)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testBool    :: Maybe Bool
testMM      :: Maybe MinMax
testSat     :: Maybe Mod7
testVM      :: Maybe Program
testInteger = testExp
testBool    = testExp
testMM      = testExp
testSat     = testExp
testVM      = testExp

newtype Program = Program VM.Program deriving Show
instance Expr Program where
  lit n = Program [VM.PushI n]
  add (Program x) (Program y) = Program (x ++ y ++ [VM.Add])
  mul (Program x) (Program y) = Program (x ++ y ++ [VM.Mul])


compile :: String -> Maybe VM.Program
compile s = parseExp lit add mul s >>= \(Program p) -> return p
