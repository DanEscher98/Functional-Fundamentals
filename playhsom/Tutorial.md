# HSoM Tutorial

1. A *note* is a *pitch* with a *duration*. Durations is measured in beats.

```haskell
note :: Dur -> Pitch -> Music Pitch
rest :: Dur -> Music Pitch
```

2. There are two operators of combination: sequencial and simultaneous. There's
   also a basic transformation function.

```haskell
(:+:) :: Music Pitch → Music Pitch → Music Pitch
(:=:) :: Music Pitch → Music Pitch → Music Pitch
trans :: Int -> Pitch -> Pitch
```

3. Axioms:
    - **enharmonics**: two notes that "sound the same", such as Gs and Af
    - `n :+: rest == n`
    - `trans i (trans j p) = trans (i + j) p`
    - `pitch (abspitch p) = p`
