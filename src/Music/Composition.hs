module Music.Composition
    (
        Scale(..),
        majorScale,
        minorScale,
        getScalePitchFrom,
        getScaleChord,
        chordProgression
    ) where

import Music.Types

import Data.Maybe (fromMaybe)

newtype Scale = Scale [Pitch]

instance Show Scale where
    show (Scale ps) = show ps

majorScale :: PitchClass -> Scale
majorScale root = Scale $ map ($ Pitch root 0) transpositions
    where
        transpositions = 
            [
                id,
                transpose 2,
                transpose 4,
                transpose 5,
                transpose 7,
                transpose 9,
                transpose 11
            ]

minorScale :: PitchClass -> Scale
minorScale root = Scale $ map ($ Pitch root 0) transpositions
    where
        transpositions = 
            [
                id,
                transpose 2,
                transpose 3,
                transpose 5,
                transpose 7,
                transpose 8,
                transpose 10
            ]

-- Get the nth pitch from the ath in the scale
getScalePitchFrom :: Scale -> Int -> Int -> Pitch
getScalePitchFrom (Scale ps) a n = let a' = a `mod` length ps
                                       b = (a' + n) `mod` length ps
                                       dOct = (a' + n) `div` length ps
                           in (\(Pitch pc o) -> Pitch pc (o + dOct)) $ ps !! b

getScaleChord :: Scale -> Int -> Int -> Duration -> Music
getScaleChord s oct n d = transpose (12 * oct) $ fromMaybe (Single (Rest Whole))
    $ parallelise $ map ((\p -> Single (Note p d)) . getScalePitchFrom s n) 
    [0, 2, 4]

chordProgression :: Scale -> Int -> Duration -> [Int] -> Music
chordProgression s oct d = fromMaybe (Single (Rest Whole)) . 
    sequentialise . map (\x -> getScaleChord s oct x d)
