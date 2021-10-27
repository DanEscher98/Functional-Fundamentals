module Algebra where

{- By convention:
    i = rows
    j = columns
-}

altOnes :: Num n => [n]
altOnes = [1,-1] ++ altOnes

remove :: Int -> [a] -> [a]
remove _ []     = error "Empty list"
remove 0 (x:xs) = xs
remove n (x:xs) = x : remove (n-1) xs

minor :: Int -> Int -> [[a]] -> [[a]]
minor i j a = map (remove j) (remove i a)

sumAlt :: Num n => [n] -> n
sumAlt []       = 0
sumAlt [x]      = x
sumAlt (x:y:xs) = x - y + sumAlt xs

det :: Num n => [[n]] -> n
det [[e]] = e
det m     = sumAlt [e * det (minor 0 j m)| (e, j) <- enumerate_m ]
    where enumerate_m = zip (head m) [0..]

transpose :: [[a]] -> [[a]]
transpose m = [ [r!!i | r <- m] | i <- [0..(length m - 1)]]

cofactor :: Num n => [[n]] -> [[n]]
cofactor m = [ [ (-1)^(i+j) * det (minor i j m) | j <- n] | i <- n]
    where n = [0..(length m - 1)]

adjoint :: Num n => [[n]] -> [[n]]
adjoint = transpose . cofactor

inverse :: Fractional n => [[n]] -> [[n]]
inverse m = [map (/ det m) r | r <- adjoint m]

dot :: Fractional n => [[n]] -> [n] -> [n]
dot a b = [ sum (zipWith (*) b a_row) | a_row <- a ]

dotM :: Num n => [[n]] -> [[n]] -> [[n]]
dotM a b = [map (sum . zipWith (*) a_row) trans_b | a_row <- a]
    where trans_b = transpose b

solveSystem :: Fractional n => [[n]] -> [n] -> [n]
solveSystem = dot . inverse
