module Main where

import qualified Data.Map as Map

type Dict = Map.Map String String

mydict = Map.fromList [
    ("where", "dove"),
    ("is", "e"),
    ("the", "il")]


translate :: Dict -> [String] -> [String]
translate dict words = map trans words where
    trans w = case Map.lookup w dict of
                (Just w') -> w'
                Nothing   -> "UNAVAILABLE"

testTranslation :: Dict -> IO ()
testTranslation dict = do
    print $ translate dict ["where", "is", "the", "colosseum"]

testInsertion :: Dict -> IO Dict
testInsertion dict = do
    return $ Map.insert "colosseum" "colosseo" dict

main = do
      testTranslation mydict
      dict'  <- testInsertion mydict
      testTranslation dict'
      putStrLn "End of program."
