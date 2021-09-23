module ListState where

type Birds = Int
type Pole = (Birds, Birds)

birdsLeft :: Pole -> Birds -> Maybe Pole
birdsLeft (bl, br) b
    | abs (bl' - br) < 4 = Just (bl', br)
    | otherwise = Nothing
    where bl' = bl + b

birdRight :: Pole -> Birds -> Maybe Pole
birdRight (bl, br) b
    | abs (bl - br') < 4 = Just (bl, br')
    | otherwise = Nothing
    where br' = br + b

banana :: Pole -> Maybe Pole
banana _ = Nothing
