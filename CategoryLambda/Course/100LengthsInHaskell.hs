module OneThousandLengths where

import           Control.Monad.State
import           GHC.Conc.Sync

-- the Prelude implementation of length
standard []     = 0
standard (x:xs) = 1 + standard(xs)


-- the bully takes the unassuming Prelude length and
-- coerces it to evaluate (+) as soon as possible
bully []     = 0
bully (x:xs) = x `seq` 1 + bully xs


-- the cadet drops and gives however n many pushups.
-- If droping n gives an empty list, the cadet as gone through the whole list
cadet xs = pushups 0 xs where
  pushups n xs
    | drop n xs == [] = n
    | otherwise = pushups (n+1) xs


-- the cameraman controls takes of a scene.  When the result of taking n
-- elements of a list == taking (n+1), then you have covered all elements
cameraman xs = camera 0 xs where
  camera n xs
    | take n xs == take (n+1) xs = n
    | otherwise = camera (n+1) xs


-- the cartographer sums a map of the list where all its elements are 1
cartographer xs = sum (map (\x -> 1) xs)


-- the clerk scans what is given to him:
-- He does a sort of currying sum of a list with elements turned to 1.
-- This means that the last applied sum (highest in the currying tree)
-- is the length of the list
clerk xs = last $ scanl (+) 0 (map (\x -> 1) xs)


-- the composer uses function composition (. operator)
-- with eta conversion (no explicit list argument)
-- to sum over a list with elements all mapped to 1
composer = sum . map (\x -> 1)


-- the comprehender sums every element -> 1 in the domain of the list
comprehender xs = sum [1 | _ <- xs]


-- the gibbon climbs the fold tree:
-- assigning each left branch to 1, and summing the left and right as he goes
gibbon xs = foldr (\x y -> (+) 1 y) 0 xs


-- the jacket uses its zipper to zip up a series of [1]s with (+)
jacket (x:xs) = (zipper (x:xs)) !! 0
                where zipper []     = [0]
                      zipper (x:xs) = zipWith (+) [1] (zipper xs)


-- the librarian indexes into his card catalog, using a monadic indexer
monadex [] _ = Nothing
monadex (x:_) 0 = Just x
monadex (_:xs) n = case monadex xs (n-1) of
  Nothing -> Nothing
  Just x  -> Just x


librarian xs = catalog xs 0 where
  catalog xs n
    | monadex xs n == Nothing = n
    | otherwise = catalog xs (n+1)


-- the origamist folds a list mapped to 1 via (+)
origamist xs = foldl (+) 0 (map (\x -> 1) xs)


-- the crown prince is the successor to the crown, and uses succ as he
-- recurses xs
prince xs = crown 0 xs where
  crown n []     = n
  crown n (x:xs) = crown (succ n) xs


-- the rock uses the hard and steady const to reassign (head list) to 1
-- and the sum is built up as the recursion exits
rock []     = 0
rock (x:xs) = (const 1 x) + rock xs


-- the sugardaddy uses right-associating infix application operator ($):
-- money, money, money
sugardaddy xs = sum $ map (\x -> 1) xs


-- the tyrant also coerces, but in a more grandiose fashion
-- by using multiple cores if possible
tyrant []     = 0
tyrant (x:xs) = x `pseq` tyrant xs `par` 1 + tyrant xs


-- the waiter waits on a tuple (list, count)
-- until it meets the requirements of lambda pred
-- a tuple is required because until is recursive
waiter xs = snd $ until (\(x,y) -> x == []) decreaselist (xs, 0)
            where decreaselist ((x:xs), n) = (xs, (n+1))
