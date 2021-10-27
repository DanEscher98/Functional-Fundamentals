module TreeMonad where
import           Control.Monad.ST

newtype ST a = S (s -> (a, s))

data Tree a = Leaf a | Node (Tree a) (Tree a)
instance (Show a) => Show (Tree a) where
    show (Leaf a)   = "[" ++ show a ++ "]"
    show (Node l r) = "(" ++ show l ++ " - " ++ show r ++ ")"

relabel :: (Tree a) -> Int -> (Tree Int, Int)
relabel (Leaf _)    n = (Leaf n, succ n)
relabel (Node l r)  n = (Node l' r', n'')
    where (l', n')  = relabel l n
          (r', n'') = relabel l n'

-- And now, using State Monads
app :: ST a -> Int -> ST Int
app (S st) s = st s

fresh :: ST Int
fresh = S (\n -> (n, succ n))

mlabel :: (Tree a) -> ST (Tree Int)
mlabel (Leaf _)     = do n <- fresh
                         return (Leaf n)
mlabel (Node l r)   = do l' <- mlabel l
                         r' <- mlabel r
                         return (Node l' r')

label :: Tree a -> Tree Int
label t = fst (app (mlabel t) 0)
