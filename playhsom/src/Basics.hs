module Basics (musicSeq, Basics.chord, arpeggio, getChord, basicHarm, chrom, getScale, defScale) where

import           Data.Vector (Vector, (!))
import qualified Data.Vector as Vec
import           Euterpea


musicSeq :: (Music Pitch -> Music Pitch -> Music Pitch)
  -> Pitch -> Dur -> Vector Int -> Music Pitch
musicSeq op p time = foldr (\v -> op . note time $ trans v p) (rest time)


chord :: Pitch -> Dur -> Vector Int -> Music Pitch
chord = musicSeq (:=:)

arpeggio :: Pitch -> Dur -> Vector Int -> Music Pitch
arpeggio = musicSeq (:+:)

getChord :: Pitch -> Dur -> Mode-> Music Pitch
getChord p time m = Basics.chord p time $ Vec.snoc (extractIdx [0, 1, 3] (Vec.fromList $ getScale m)) 12

basicHarm :: Pitch -> Vector Int -> Music Pitch
basicHarm p vs = Basics.chord p 1 vs :=:
  arpeggio p (1 / fromIntegral (length vs)) vs

chrom :: Pitch -> Pitch -> Dur -> Music Pitch
chrom p1 p2 time = case compare p1 p2 of
  EQ -> note qn p1
  LT -> aux [(absPitch p1) .. (absPitch p2)]
  GT -> aux . reverse $ [(absPitch p2) .. (absPitch p1)]
  where
    aux :: [AbsPitch] -> Music Pitch
    aux = foldr ((:+:) . note time . pitch) (rest qn)

getScale :: Mode -> [Int]
getScale m = case m of
  Major                      -> getScale Ionian
  Minor                      -> getScale Aeolian
  Ionian                     -> [0, 2, 4, 5, 7, 9, 11]
  Dorian                     -> [0, 2, 3, 5, 7, 9, 10]
  Phrygian                   -> [0, 1, 3, 5, 7, 8, 10]
  Lydian                     -> [0, 2, 4, 6, 7, 9, 11]
  Mixolydian                 -> [0, 2, 4, 5, 7, 9, 10]
  Aeolian                    -> [0, 2, 3, 5, 7, 8, 10]
  Locrian                    -> [0, 1, 3, 5, 6, 8, 10]
  CustomMode "Adonai Malach" -> [0, 2, 4, 5, 7, 9, 10]
  CustomMode "Ahavah Rabbah" -> [0, 1, 4, 5, 7, 8, 10]
  CustomMode "Magein Avot"   -> getScale Aeolian
  _                          -> []

defScale :: Pitch -> Dur -> [Int] -> Music Pitch
defScale p time = foldr ((:+:) . note time . pitch . (+) (absPitch p)) (rest time)


extractIdx :: [Int] -> Vector a -> Vector a
extractIdx indexes xs = Vec.backpermute xs (Vec.fromList indexes)

-- play $ defScale (C,4) (1/8) (getScale Locrian)

-- foldr (:+:) (rest qn) $ map (\pa -> note qn (pitch pa)) . reverse $ [a..b]
--
-- (\p -> foldr ((:+:) . note qn . pitch . (+) p) (rest qn) $ getScale (CustomMode "Adonai Malach")) $ absPitch (C,5)
