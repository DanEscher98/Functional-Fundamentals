module AlgorithmicMusic
  ( prefix,
    mel1,
    mel2,
  )
where

import           Euterpea



prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x : xs) =
  let n pn = x : pn
   in [x] : map n (prefixes xs)

prefix :: [Music a] -> Music a
prefix mel =
  let m1 = line (concat (prefixes mel))
      m2 = transpose 12 (line (concat (prefixes (reverse mel))))
      m = instrument Flute m1 :=: instrument AcousticGrandPiano m2
   in m :+: transpose 5 m :+: m

mel1, mel2 :: [Music Pitch]
mel1 = [c 5 en, e 5 sn, g 5 en, b 5 sn, a 5 en, f 5 sn, d 5 en, b 4 sn, c 5 en]
mel2 = [c 5 sn, e 5 sn, g 5 sn, b 5 sn, a 5 sn, f 5 sn, d 5 sn, b 4 sn, c 5 sn]


