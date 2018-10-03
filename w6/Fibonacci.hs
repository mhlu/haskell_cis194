{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- ex 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- ex 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- ex 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance (Show a) => Show (Stream a) where
    show = show . take 20 . streamToList

-- ex 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = let x' = f x
                    in Cons x' (streamFromSeed f x')

-- slower alternative
-- streamFromSeed' :: (a -> a) -> a -> Stream a
-- streamFromSeed' f x = Cons (f x) (streamMap f $ streamFromSeed' f x )

-- ex 5
nats :: Stream Integer
nats = streamFromSeed (+1) (-1)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x ( interleaveStreams ys xs )

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (gen 0)
    where gen n = interleaveStreams (streamRepeat (2^n)) (gen (n+1))

-- ex 6
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger n = Cons n $ streamRepeat 0
    negate (Cons x xs) = Cons (negate x) $ negate xs
    (Cons x xs) + (Cons y ys) = Cons (x+y) (xs+ys)
    as@(Cons a as') * bs@(Cons b bs') = Cons (a*b) ( streamMap (a*) bs' + as'*bs)

instance Fractional (Stream Integer) where
    as@(Cons a as') /  bs@(Cons b bs') = q
        where q = Cons (a `div` b) ( streamMap ((1`div`b)*) (as' - q*bs') )

fibs3 :: Stream Integer
fibs3 = x / ( 1 - x - x^2 )

-- ex 7
data Matrix4x4 = Matrix4x4 {a00::Integer,
                            a01::Integer,
                            a10::Integer,
                            a11::Integer}
    deriving (Show, Eq)

fib4 :: Integer -> Integer
fib4 n
    | n == 0 = 1
    | otherwise = a01 $ (Matrix4x4 1 1 1 0) ^ n


instance Num Matrix4x4 where
    (Matrix4x4 a00 a01 a11 a10) * (Matrix4x4 b00 b01 b11 b10) =
        Matrix4x4 (a00*b00+a01*b10) (a00*b01+a01*b11) (a10*b00+a11*b10) (a10*b01+a11*b11)
