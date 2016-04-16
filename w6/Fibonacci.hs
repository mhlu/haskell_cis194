module Fibonacci where

fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer] 
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

fibs3 :: [Integer]
fibs3 = 0 : 1 : next fibs3
    where next (a:xs@(b:_)) = (a+b) : next xs

data Stream a = a `Cons` Stream a

streamToList :: Stream a -> [a]
streamToList (x `Cons` s) = x : streamToList s

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat a = a `Cons` streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (s `Cons` ss) = f s `Cons` streamMap f ss

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = a `Cons` streamFromSeed f b
    where b = f a

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = startRuler 0
    where startRuler y = interleaveStreams (streamRepeat y) (startRuler (y+1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (a `Cons` as) (b `Cons` bs) = a `Cons` (b `Cons` interleaveStreams as bs)


