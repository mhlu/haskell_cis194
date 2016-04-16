{-# OPTIONS_GHC -Wall #-}

-- Question 1 ( credit card validation )
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = ys
     where (_, ys) = foldr (\x (t, acc) -> if t then (not t, x*2 : acc) else (not t, x : acc ) ) (False, []) xs

sumDigits :: [Integer] -> Integer
sumDigits =  sum . map (sum . toDigits ) 

validate :: Integer -> Bool
validate n = s `mod` 10 == 0
    where s = sumDigits . doubleEveryOther . toDigits $ n

-- Question 2 ( tower of hanoi )
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n fro to via
    | n < 1 = []
    | otherwise = hanoi (n-1) fro via to ++ [(fro, to)] ++ hanoi (n-1) via to fro
