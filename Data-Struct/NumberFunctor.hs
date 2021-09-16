module NumeroMonadico where

data Quizas a = Nada | Un a
instance (Show a) => Show (Quizas a) where
    show Nada   = "¯\\_(ツ)_/¯"
    show (Un x) = "(✿ ◠‿◠)-☆ " ++ show x

-- Definition equivalent to binding
(~>) :: Expresion -> (Int -> Quizas Int) -> Quizas Int
e ~> f  = unir (evalua e) f
    where unir Nada   _ = Nada
          unir (Un a) f = f a

data Expresion
    = Valor Int
    | Add Int Int
    | Mul Int Int
    | Sub Int Int
    | Qot Int Int

evalua :: Expresion -> Quizas Int
evalua (Valor a) = Un a
evalua (Add a b) = Un (a + b)
evalua (Mul a b) = Un (a * b)
evalua (Sub a b) = sub a b
evalua (Qot a b) = qot a b

puro :: Int -> Quizas Int
puro = Un

sub :: Int -> Int -> Quizas Int
sub a 0 = Un a
sub a b
    | a < b     = Nada
    | otherwise = Un (a - b)

qot :: Int -> Int -> Quizas Int
qot _ 0 = Nada
qot 0 _ = Un 0
qot a 1 = Un a
qot a b = Un (auxQot a b) where
    auxQot a b =
        if a < b
            then 0
            else succ (auxQot (a-b) b)

-- (Qot 12 3) ~> (\a ->
-- (Add a 2) ~> (\b ->
-- (Mul a b) ~> (\c ->
-- puro c)))
