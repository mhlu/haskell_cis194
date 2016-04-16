module Higher where

import Data.List

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum
      . filter even
      . takeWhile (/= 1)
      . iterate (\n -> if even n then n `div` 2 else 3*n+1)

data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree (x:xs) = Node height lhs x rhs
    where lhs = foldTree $ take half xs
          rhs = foldTree $ drop half xs
          half = length xs `div` 2
          h (Node i _ _ _ ) = i
          h Leaf = (-1)
          height = 1 + max (h lhs) (h rhs)

xor :: [Bool] -> Bool
xor = foldl (\acc x -> if x then not acc else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x acc -> acc . (`f` x) ) id xs base

myFoldr :: (b -> a -> a) -> a -> [b] -> a
myFoldr f base xs = foldl (\acc x -> acc . (f x)) id xs base

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ [1..n] \\ notPrime
    where p = filter (uncurry (<=)) $ cartProd [1..n] [1..n]
          notPrime = map (\(x, y) -> x+y+2*x*y) p
