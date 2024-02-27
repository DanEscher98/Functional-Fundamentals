module RandomMusic (
  randInts,
  randIntsRange,
  melGen,
  somethingWeird,
  randomMel,
  duet
) where
import           Euterpea
import           System.Random (StdGen, mkStdGen, random, randomR)

randInts :: Int -> [Int]
randInts seed = recInts (mkStdGen seed) where
  recInts g = let (i, g') = random g in i : recInts g'

randIntsRange :: (Int, Int) -> Int -> [Int]
randIntsRange (lower, upper) =
  map (\i -> (i `mod` (upper - lower)) + lower) . randInts

melGen :: Int -> Music (Pitch, Volume)
melGen s =
  let pitches = map pitch $ randIntsRange (30, 80) s
      vols = randIntsRange (40, 100) (s+1)
  in line $ zipWith (curry (note sn)) pitches vols

somethingWeird =
  let part1 = instrument Lead6Voice $ dim $ rit $ cut 6 $ melGen 345
      part2 = instrument Marimba $ cut 4 $ melGen 234
      part3 = instrument TubularBells $ cre $ acc $ cut 8 $ melGen 789
  in  chord [part1, part2, part3] where
    rit = Modify (Phrase [Tmp $ Ritardando 0.5])
    acc = Modify (Phrase [Tmp $ Accelerando 0.5])
    dim = Modify (Phrase [Dyn $ Diminuendo 0.5])
    cre = Modify (Phrase [Dyn $ Crescendo 0.5])


choose :: [a] -> StdGen -> (a, StdGen)
choose [] _ = error "Nothing to choose from!"
choose xs gn =
  let (i, gn') = random gn
  in  (xs !! (i `mod` length xs), gn')

randomMel :: [AbsPitch] -> [Dur] -> Volume -> StdGen -> Music (AbsPitch, Volume)
randomMel pitches durs thresh g0 =
  let (p, g1) = choose pitches g0
      (t, g2) = choose durs g1
      (v, g3) = randomR (0,127) g2
      x = if v < thresh then rest t else note t (p,v)
  in  x :+: randomMel pitches durs thresh g3

pitches1, pitches2 :: [AbsPitch]
pitches1 = [60,62,63,65,67,68,70,72,74,75,77,79] -- C-minor
pitches2 = [36,43,46,48] -- also C-minor (root, 5th, 7th, root)

mel1, mel2, duet :: Music (AbsPitch, Volume)
mel1 = randomMel pitches1 [qn,en,en,en] 40 (mkStdGen 500)
mel2 = randomMel pitches2 [hn,qn,qn,qn] 20 (mkStdGen 501)
duet = tempo 1.75 (instrument Banjo mel1 :=: instrument Celesta mel2)
