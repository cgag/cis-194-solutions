module HW4 where

fun1 :: [Integer] -> Integer
fun1 = product .  map (\x -> x - 2) . filter even

-- Damn this problem
-- fun2 :: Integer -> Integer
-- fun2 n = undefined
--
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- use foldr to generate alanced binary tree from list of input values
-- Why foldr and not foldl?
foldTree :: [a] -> Tree a
foldTree = foldr insertBalanced Leaf
  where
    insertBalanced newA Leaf = Node 0 Leaf newA Leaf
    insertBalanced newA (Node i l a r)
      | height l < height r = Node (i + 1) (insertBalanced newA l) a r
      | otherwise           = Node (i + 1) l a (insertBalanced newA r)

height :: Tree a -> Integer
height Leaf = 0
height (Node _ l _ r) = 1 + max (height l) (height r)


-- Not sure if I'm cheating by using odd and not just a fold
xor :: [Bool] -> Bool
xor = odd . foldl (\acc bool -> if bool then acc + 1 else acc) (0 :: Integer)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- myFoldl = (a -> b -> a) -> a -> [b] -> a

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (+1) . map (*2) . filter (\x -> (x `notElem` cards)) $ [1..n]
  where
    cards = map (\(i,j) -> i + j + (2*i*j)) $ cartProd [1..n] [1..n]
    cartProd :: [a] -> [b] -> [(a, b)]
    cartProd xs ys = [(x,y) | x <- xs, y <- ys]
