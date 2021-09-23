module NumeroDo where

data Numero = Zero | S Numero
instance Show Numero where
    show Zero  = "0"
    show (S a) = "S" ++ show a

add :: Numero -> Numero -> Maybe Numero
add a Zero  = Just a
add a (S b) = add (S a) b

mul :: Numero -> Numero -> Maybe Numero
mul Zero _  = Just Zero
mul _ Zero  = Just Zero
mul a (S b) = mul a b >>= add a

dec :: Numero -> Maybe Numero
dec Zero  = Nothing
dec (S x) = Just x

eq :: Numero -> Numero -> Bool
eq Zero Zero   = True
eq (S _) Zero  = False
eq Zero (S _)  = False
eq (S a) (S b) = eq a b

menor :: Numero -> Numero -> Bool
menor a Zero      = False
menor Zero (S _)  = True
menor (S a) (S b) = menor a b

sub :: Numero -> Numero -> Maybe Numero
sub Zero _      = Nothing
sub a Zero      = Just a
sub (S a) (S b) = sub a b

qot :: Numero -> Numero -> Maybe Numero
qot _ Zero = Nothing
qot Zero _ = Just Zero
qot a b | eq a b    = Just (S Zero)
        | menor a b = Just Zero
        | otherwise = do
            x <- sub a b
            y <- qot x b
            return (S y)

intToSN :: Int -> Maybe Numero
intToSN 0 = Just Zero
intToSN n
    | n < 0     = Nothing
    | otherwise = do
        x <- intToSN (pred n)
        return (S x)

operations :: Maybe Numero
operations = do
    n1 <- intToSN 24
    n2 <- intToSN 4
    n3 <- intToSN 5
    a <- qot n1 n2
    b <- qot a n3
    mul b (S(S Zero))

main :: IO ()
main = print operations
