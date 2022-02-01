module BowlingGame where


allPins :: Int -> Bool
allPins = (==10)

moveFrame :: Int -> [Int] -> [Int]
moveFrame = drop

scoreRolls :: Int -> [Int] -> Int
scoreRolls numRolls rolls = sum $ take numRolls rolls

isStrike :: [Int] -> Bool
isStrike = allPins . scoreRolls 1

isSpare :: [Int] -> Bool
isSpare = allPins . scoreRolls 2

scoreStrike :: ([Int], [Int]) -> ([Int], [Int])
scoreStrike (frameScores, rolls) =
    (frameScores ++ [scoreRolls 3 rolls], moveFrame 1 rolls)

scoreSpare :: ([Int], [Int]) -> ([Int], [Int])
scoreSpare (frameScores, rolls) =
    (frameScores ++ [scoreRolls 3 rolls], moveFrame 2 rolls)

scoreNormal :: ([Int], [Int]) -> ([Int], [Int])
scoreNormal (frameScores, rolls) =
    (frameScores ++ [scoreRolls 2 rolls], moveFrame 2 rolls)

scoreFrame :: ([Int], [Int]) -> Int -> ([Int], [Int])
scoreFrame state@(frameScores, rolls) _frame
    | isStrike rolls = scoreStrike state
    | isSpare rolls = scoreSpare state
    | otherwise = scoreNormal state

score :: [Int] -> Int
score rolls = sum . fst . foldl scoreFrame ([], rolls) $ [1..10]
