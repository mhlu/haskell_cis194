module Higher where

import Data.List ( (\\) )

-- ex 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum .
        filter even .
        takeWhile (/=1) .
        iterate (\n -> if even n then n `div` 2 else 3 * n + 1 )

-- ex 2

data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _ ) = h

depth :: Tree a -> Tree a -> Integer
depth a b = 1 + max (height a) (height b)

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr insert Leaf

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x t@(Node h l v r )
    | x < v = rotate $ Node (depth newLeft r) newLeft v r
    | x > v = rotate $ Node (depth l newRight ) l v newRight
    | otherwise = t
    where newLeft = (insert x l)
          newRight = (insert x r)

rotate :: Tree a -> Tree a
rotate t@(Node h l@(Node lh ll _ lr) v r)
    | lh - height r > 1 && height ll > height lr = rightRotate $ t
    | lh - height r > 1 = rightRotate $ (Node h (leftRotate l) v r )
rotate t@(Node h l _ r@(Node rh rl v rr))
    | rh - height l > 1 && height rr > height rl = leftRotate $ t
    | rh - height l > 1 = leftRotate $ (Node h l v (rightRotate r))
rotate t = t

-- this part is hard to understand unless you workout it out in a drawing
rightRotate :: Tree a -> Tree a
rightRotate (Node h (Node lh ll lv lr)  v r) = Node lh ll lv (Node (depth lr r)  lr v r )

leftRotate :: Tree a -> Tree a
leftRotate (Node h l v (Node rh rl rv rr)) = Node rh (Node (depth l rl) l v rl ) rv rr

printTree :: (Show a ) => Tree a -> String
printTree Leaf = "Empty"
printTree t = unlines ( printHelper t 0 )
    where printHelper (Node h Leaf v Leaf) lv = [pad lv ++ show (h, v)]
          printHelper (Node h l v r) lv = (printHelper l (lv+1)) ++ [pad lv ++ show (h, v)] ++ (printHelper r (lv+1))
          pad lv = replicate (lv*4) ' '

-- ex 3
xor :: [Bool] -> Bool
xor = foldr (\a b -> (not a && b) || (a && not b)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []


-- bonus
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x accFcn -> accFcn . (`f` x) ) id xs base

myFoldr :: (b -> a -> a) -> a -> [b] -> a
myFoldr f base xs = foldl (\accFcn x -> accFcn . (x `f`) ) id xs base

-- ex 4
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x+1) .
                foldr (\(i, j) acc -> if i <= j && (i+j+2*i*j) <= n then filter (/=(i+j+2*i*j)) acc else acc) [0..n] $
                (cartProd [1..n] [1..n])

-- this is faster since not every element of the prod have to be compared with the [0..n] due to shortcircuiting behavior of (\\)
sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = map ((+1) . (*2)) ( [0..n] \\ notPrime )
    where pairs = filter (uncurry (<=)) ( cartProd [1..n] [1..n] )
          notPrime = filter (<n) ( map (\(x, y) -> x+y+2*x*y) pairs )


