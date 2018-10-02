{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Parser
import qualified ExprT as E
import qualified StackVM as VM
import qualified Data.Map as M

-- Q1
eval :: E.ExprT -> Integer
eval (E.Lit n) = n
eval (E.Add e1 e2) = (eval e1) + (eval e2)
eval (E.Mul e1 e2) = (eval e1) * (eval e2)

-- Q2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp E.Lit E.Add E.Mul

-- Q3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr E.ExprT where
    lit = E.Lit
    add = E.Add
    mul = E.Mul

reify :: E.ExprT -> E.ExprT
reify = id

-- Q4
instance Expr Integer where
    lit = id
    add n1 n2 = n1 + n2
    mul n1 n2 = n1 * n2

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

-- newtype
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit n = MinMax n
    add (MinMax n1) (MinMax n2) = MinMax (max n1 n2)
    mul (MinMax n1) (MinMax n2) = MinMax (min n1 n2)

instance Expr Mod7 where
    lit n = Mod7 (n `mod` 7)
    add (Mod7 n1) (Mod7 n2) =  Mod7 ( (n1 + n2) `mod` 7 )
    mul (Mod7 n1) (Mod7 n2) =  Mod7 ( (n1 * n2) `mod` 7 )

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- Q5

instance Expr VM.Program where
    lit n = [VM.PushI n]
    add exp1 exp2 = exp1 ++ exp2 ++ [VM.Add]
    mul exp1 exp2 = exp1 ++ exp2 ++ [VM.Mul]


compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

-- Q6

class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
    | Var String
    | Add VarExprT VarExprT
    | Mul VarExprT  VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n = \_ -> Just n
    add e1 e2 = \d -> case (e1 d, e2 d) of
                        (Just n1, Just n2) -> Just (n1+n2)
                        (_, _) -> Nothing
    mul e1 e2 = \d -> case (e1 d, e2 d) of
                        (Just n1, Just n2) -> Just (n1*n2)
                        (_, _) -> Nothing

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

