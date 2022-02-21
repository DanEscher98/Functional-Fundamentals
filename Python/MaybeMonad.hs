module MaybeMonad where

import           Control.Exception
import           Control.Monad.IO.Class
import           System.IO.Unsafe

data Quizas a = Nada | Un a

instance Show a => Show (Quizas a) where
    show Nada   = "Nada"
    show (Un a) = "Un" ++ show a

instance Functor Quizas where
    fmap f Nada   = Nada
    fmap f (Un a) = Un (f a)

instance Applicative Quizas where
    pure x = Un x
    Nada <*> _        = Nada
    _ <*> Nada        = Nada
    (Un f) <*> (Un x) = Un (f x)

instance Monad Quizas where
    (>>=) :: Quizas a -> (a -> Quizas b) -> Quizas b
    Nada >>= _   = Nada
    (Un x) >>= f = f x


parseInt :: String -> Either SomeException Int
parseInt s = unsafePerformIO . (try $ (evaluate (read s :: Int)) :: IO (Either SomeException Int))

add = (\x -> x + 3)
neg = (\x -> (-x))
proc :: String -> Quizas String
proc x = Un x >>= (\y -> int y >>= (return . show . neg . add))


main :: IO ()
main = do
    let x = "123"
    print . proc $ x
