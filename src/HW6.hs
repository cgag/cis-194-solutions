module HW6 where

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = Cons 0 (interleaveStreams
                  (interleaveStreams  (streamRepeat 1)
                                      (streamFromSeed (+1) 2))
                  (streamRepeat 0))
  where
    interleaveStreams :: Stream a -> Stream a -> Stream a
    interleaveStreams (Cons x s1) (Cons y s2) =
      Cons x (Cons y (interleaveStreams s1 s2))
