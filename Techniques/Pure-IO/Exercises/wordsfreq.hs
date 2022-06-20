module Main where

import           Data.Char (toLower)
import           Data.List (sortBy)
import qualified Data.Map  as Map

type Index = Map.Map String Int

-- implement a function to count words
indexWords :: Index -> [String] -> Index
indexWords index = foldl acc index where
    acc :: Index -> String -> Index
    acc i word = let n = Map.findWithDefault 0 word i
                  in Map.insert word (succ n) i

splitWords :: String -> [String]
splitWords = words . map (\c -> if elem c ".,;-\n"
                                   then ' '
                                   else toLower c)

mostFrequent :: [String] -> [(String, Int)]
mostFrequent words =
    let index = indexWords Map.empty words
     in take 10 (sortBy cmpFreq (Map.toList index)) where
         cmpFreq :: (String, Int) -> (String, Int) -> Ordering
         cmpFreq (w1, n1) (w2, n2) = compare n2 n1

main = do
    text <- readFile "moby.txt"
    print . mostFrequent . splitWords $ text
