module Golf where

import Data.List


skips :: [a] -> [[a]]
skips xs = map (flip skip xs) $ [1..length xs]
    where skip n = map snd . filter ((==n) . fst ) . zip (cycle [1..n])

{- this version is shorter

skips :: [a] -> [[a]]
skips xs = map (skip xs) [1..length xs]
    where skip xs n = case drop (n-1) xs of
                (y:ys) -> y : skip ys n
                [] -> [] -}

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs)
    | b > a && b > c = b : localMaxima (c:xs)
    | otherwise = localMaxima (b:c:xs)
localMaxima _ = []

{-
localMaxima :: [Integer] -> [Integer]
localMaxima xs@(x:_) = map fst . filter (snd) . zip xs . zipWith (&&) left $ right
    where left = zipWith (>) xs (x:init xs)
          right = zipWith (>) xs (tail xs) ++ [False] -}


histogram :: [Integer] -> String
histogram xs = (unlines . transpose $ cols ) ++ "==========\n0123456789\n"
    where count xs n = length . filter (==n) $ xs
          counts = map (count xs) [0..9]
          maxCnt = maximum counts
          col n = replicate (maxCnt - n) ' ' ++ replicate n '*'
          cols = map col counts
