module ListasInfinitas where

-- import           Test.QuickCheck

-- 7.6 Es la lista formada por listas de elementos
-- consecutivos de la lista xs
agrupa :: Int -> [a] -> [[a]]
agrupa n = takeWhile (not . null)
            . map (take n)
            . iterate (drop n)

-- prop_AgrupaLong :: Int -> [Int] -> Property
-- prop_AgrupaLong n xs =
--     n > 0 && not (null gs) ==>
--         and [length g == n | g <- init gs] &&
--             0 < length (last gs) && length (last gs) <= n
--                 where gs = agrupa n xs

primos :: Integral a => [a]
primos = criba [2..] where
    criba (n:ns) = n : criba [x | x <- ns, mod x n /= 0]

sumaDeDosPrimos :: Int -> [(Int, Int)]
sumaDeDosPrimos n = [
    (x, n-x) | x <- primosN,
    x < n-x, elem (n-x) primosN]
        where primosN = takeWhile (<n) primos
