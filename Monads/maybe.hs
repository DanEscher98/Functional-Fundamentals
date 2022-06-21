module Quizas where

data Quizas a = Nada | Un a

return :: a -> Quizas a
return = Un

fmap :: (a -> b) -> Quizas a -> Quizas b
fmap _ Nada   = Nada
fmap f (Un x) = Un (f x)

join :: Quizas (Quizas a) -> Quizas a
join Nada   = Nada
join (Un x) = x

