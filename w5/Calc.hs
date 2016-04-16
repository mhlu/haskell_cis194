{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add t1 t2) = eval t1 + eval t2
eval (Mul t1 t2) = eval t1 * eval t2

evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
    Nothing -> Nothing
    Just e -> Just $ eval e

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit n
        | n <= 0 = False
        | otherwise = True
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax $ max a b
    mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
    lit n = Mod7 $ n `rem` 7
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `rem` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `rem` 7

    

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)


instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- ghci test     
{-testExp :: Expr a => Maybe a-}
{-testExp = parseExp lit add mul "(3 * -4) + 5"-}
{-testInteger = testExp :: Maybe Integer-}
{-testBool = testExp :: Maybe Bool-}
{-testMM = testExp :: Maybe MinMax-}
{-testSat = testExp :: Maybe Mod7-}

-- exercise 5 --
instance Expr StackVM.Program where
    lit i = [StackVM.PushI i]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

