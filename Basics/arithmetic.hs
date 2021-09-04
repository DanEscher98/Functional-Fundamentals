module Aritmetica where
import           Data.Maybe (fromJust)
-- :l file

-- Equivalente a (+)
suma :: Int -> Int -> Int
suma a 0 = a
suma a n = succ (suma a (n-1))

resta :: Int -> Int -> Int
resta a 0 = a
resta a n = pred (resta a (n-1))

-- Equivalente a (*)
producto :: Int -> Int -> Int
producto a 1 = a
producto a n = suma a (producto a (n-1))

-- Devuelve la parte entera de la división
cociente :: Int -> Int -> Int
cociente a b
    | a < b = 0
    | otherwise = succ (cociente (resta a b) b)

-- Equivalente a (^)
potencia :: Int -> Int -> Int
potencia a 0 = 1
potencia a 1 = a
potencia a n = producto a (potencia a (n-1))

-- Devuleve una tupla, cociente y residuo
division :: Int -> Int -> Maybe (Int, Int)
division _ 0 = Nothing
division a b = Just (auxDiv 0 a b) where
    auxDiv q a b
        | a > b     = auxDiv (succ q) (a-b) b
        | otherwise = (q, a)

quotient a b = fst (fromJust (division a b))
reminder a b = snd (fromJust (division a b))

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (pred n)

permut :: Int -> Int -> Int
permut n k = div (factorial n) (factorial (n-k))

permutaciones :: Int -> Int -> Int
permutaciones n k = auxP n (n-k) where
    auxP a b
        | a <  b    = 0
        | a == b    = 1
        | otherwise = a * auxP (pred a) b
