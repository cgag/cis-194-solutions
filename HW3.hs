module HW3 where

skips :: [a] -> [[a]]
skips [] = []
skips xs = map (\n -> takeNths n xs) [1..length xs]


takeNths :: Int -> [a] -> [a]
takeNths _ [] = []
takeNths n xs = let rest = drop (n - 1) xs
                in take 1 rest ++ takeNths n (drop 1 rest)
