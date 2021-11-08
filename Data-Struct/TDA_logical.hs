module TDALogical where

data Prop
    = Var Char
    | Const Bool
    | Neg Prop
    | Conj Prop Prop
    | Impl Prop Prop

instance Show Prop where
    show (Var c)      = show c
    show (Const b)    = show b
    show (Neg p)      = "¬" ++ show p
    show (Conj p1 p2) = "(" ++ show p1 ++ " ∧ " ++ show p2 ++ ")"
    show (Impl p1 p2) = "(" ++ show p1 ++ " → " ++ show p2 ++ ")"

type Interpretation = [(Char, Bool)]

value :: Interpretation -> Prop -> Bool
value _ (Const b)  = b
value i (Var x)    = applyVal x i
value i (Neg p)    = not (value i p)
value i (Conj p q) = value i p && value i q
value i (Impl p q) = value i p <= value i q

applyVal :: Eq c => c -> [(c, v)] -> v
applyVal e i = head [v | (e', v) <- i, e == e']

variables :: Prop -> [Char]
variables (Const _)  = []
variables (Var x)    = [x]
variables (Neg p)    = variables p
variables (Conj p q) = variables p ++ variables q
variables (Impl p q) = variables p ++ variables q
