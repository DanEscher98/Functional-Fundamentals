module IntMonadico where

data Quizas a = Nada | Un a
instance (Show a) => Show (Quizas a) where
    show Nada   = "¯\\_(ツ)_/¯"
    show (Un x) = "(✿ ◠‿◠)-☆ " ++ show x

-- Definition equivalent to binding
(>>>) :: Quizas Int -> (Int -> Quizas Int) -> Quizas Int
Nada   >>> _ = Nada
(Un x) >>> f = f x

puro :: Int -> Quizas Int
puro = Un

-- Funciones Aritmeticas

add :: Int -> Int -> Quizas Int
add a b = Un (a+b)

mul :: Int -> Int -> Quizas Int
mul a b = Un (a*b)

sub :: Int -> Int -> Quizas Int
sub a b = Un (a-b)

qot :: Int -> Int -> Quizas Int
qot _ 0 = Nada
qot a b = Un (div a b)

-- (qot 12 3) >>> (\a ->
-- (add a 2)  >>> (\b ->
-- (mul a b)  >>> (\c ->
-- puro c)))
