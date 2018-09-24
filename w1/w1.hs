import Data.Char

-- q1
toDigits :: Integer -> [Integer]
toDigits = map ( toInteger . digitToInt ) . show
{- toDigits = reverse . toDigitsRev -}

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n == 0  = [0]
    | n `div` 10 == 0 = [ n `mod` 10 ]
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1,2]) . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . map sum . map toDigits

validate :: Integer -> Bool
validate n
    | n > 0 = (checksum n `mod` 10 ) == 0
    | otherwise = False
    where checksum = sumDigits . doubleEveryOther . toDigits

-- q2
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n fro via to = ( hanoi (n-1) via fro to ) ++
                    [(fro, to)] ++
                    ( hanoi (n-1) fro to via )

-- bonus
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 fro _ _ to = [(fro, to)]
hanoi4 n fro v1 v2 to = (hanoi4 (n-2)  v2 v1 fro to ) ++
                        [(v1, to), (fro, to), (fro, v1)] ++
                        (hanoi4 (n-2) fro v1 to v2)
