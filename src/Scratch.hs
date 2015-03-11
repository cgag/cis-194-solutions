{-# Language GeneralizedNewtypeDeriving #-}

module Scratch where

import Control.Applicative hiding (ZipList)

(.+) :: (Applicative f) => f Int -> f Int -> f Int
(.+) = liftA2 (+)

(.*) :: (Applicative f) => f Int -> f Int -> f Int
(.*) = liftA2 (*)

data Employee = Employee {
    name :: String
,   number :: String
} deriving Show

names :: [String]
names = ["Curtis", "Morgan", "Drew"]

numbers :: [String]
numbers = ["1", "2", "3"]

m1 :: [Int]
m1 = ([4,5] .* pure 2) .+ [6,1]

newtype ZipList a = ZipList { unZipList :: [a] }
    deriving (Eq, Show, Functor)

instance Applicative ZipList where
    pure = ZipList . repeat
    ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)


employees2 :: [Employee]
employees2 = unZipList $ Employee <$> ZipList names <*> ZipList numbers

data BigRecord = BR { getName  :: String
                    , getPhone :: String
                    , getIrrev :: String }
    deriving Show

r :: BigRecord
r = BR "Curtis" "4030983" "xxx"

getEmp :: BigRecord -> Employee
getEmp = Employee <$> getName <*> getPhone


-- Hurray algebraic reasoning
--  Employee <$> getName <*> getPhone
-- (Employee . getName) <*> getPhone
-- \e -> ((Employee . getName) e) (getPhone e)
-- (Empoyee name) (phone)


ex01 :: Employee
ex01 = (getEmp r) {name = "lol"}

(.*>) :: Applicative f => f a -> f b -> f b
(.*>) = liftA2 (const id)

-- Nothing *> Just 10
-- (const id) <$> Nothing <*> Just 10

main :: IO ()
main = putStrLn "hello"
