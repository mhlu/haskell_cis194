module Golf where
import Data.List

skips :: [a] -> [[a]]
skips xs = map (skip xs) [1..length xs]
    where skip xs n = case drop (n-1) xs of
                (y:ys) -> y : skip ys n
                [] -> []


localMaxima :: [Integer] -> [Integer]
localMaxima = reverse . sort . getMaxs
    where getMaxs (a:b:c:xs) 
            | b > a && b > c = b : getMaxs (c:xs)
            | otherwise = getMaxs (b:c:xs)            
          getMaxs _ = []

histogram :: [Integer] -> String
histogram xs = (unlines . transpose $ hori) ++ "==========\n0123456789\n"
    where count xs n = length . filter (==n) $ xs
          counts = map (count xs) [0..9]
          maxCount = maximum counts
          hori = map (\n -> reverse $ replicate n '*' ++ replicate (maxCount - n) ' ') counts
