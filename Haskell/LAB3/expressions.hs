-- Expressions

import Data.Maybe
import Data.Either

data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add x y) = (eval1 x) + (eval1 y)
eval1 (Sub x y) = (eval1 x) - (eval1 y)
eval1 (Mul x y) = (eval1 x) * (eval1 y)
eval1 (Div x y) = (eval1 x) `div` (eval1 y)

eval2 :: Expr -> Maybe Int
eval2 (Val x) = return x
eval2 (Add x y) = do
    i <- eval2 x
    j <- eval2 y
    Just (i + j)
eval2 (Sub x y) = do 
    i <- eval2 x
    j <- eval2 y
    Just (i - j)
eval2 (Mul x y) = do
    i <- eval2 x
    j <- eval2 y
    Just (i * j)
eval2 (Div x y) = do
    i <- eval2 x
    j <- eval2 y
    if (j == 0) then Nothing
    else Just (i `div` j)

eval3 :: Expr -> Either String Int
eval3 (Val x) = return x
eval3 (Add x y) = do
    i <- eval3 x
    j <- eval3 y
    Right (i + j)
eval3 (Sub x y) = do 
    i <- eval3 x
    j <- eval3 y
    Right (i - j)
eval3 (Mul x y) = do
    i <- eval3 x
    j <- eval3 y
    Right (i * j)
eval3 (Div x y) = do
    i <- eval3 x
    j <- eval3 y
    if (j == 0) then Left "div0"
    else Right (i `div` j)
