module Numeros where

data Numero = Zero | S Numero
instance Show Numero where
    show Zero  = "0"
    show (S x) = "S" ++ show x

prede :: Numero -> Numero
prede Zero  = Zero
prede (S n) = n

suma :: Numero -> Numero -> Numero
suma Zero b  = b
suma (S a) b = suma a (S b)

subs :: Numero -> Numero -> Numero
subs Zero b      = b
subs _ Zero      = Zero
subs (S a) (S b) = subs a b

