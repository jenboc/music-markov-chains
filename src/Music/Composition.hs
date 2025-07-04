module Music.Composition
    (
        Scale(..),
        majorScale,
        minorScale
    ) where

import Music.Types

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
