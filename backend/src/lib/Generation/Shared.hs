module Generation.Shared
    (
        nullMusic,
        chooseFromProbList
    ) where

import Music
import Data.Functor
import System.Random (randomRIO)

-- Representing an empty music ADT
nullMusic :: Music
nullMusic = Single $ Rest Sixteenth

-- Given a list of a's and their probabilities
-- pick one.
chooseFromProbList :: [(a,Double)] -> IO a
chooseFromProbList l = randomRIO (dble 0, dble 1) <&> \p -> walk 0 p l
    where
        dble :: Int -> Double
        dble n = fromIntegral n :: Double

        walk :: Double -> Double -> [(a,Double)] -> a
        walk _ _ [] = error "It was empty!"
        walk _ _ [(a,_)] = a
        walk acc p ((a,b):r) = let acc' = acc + b in
            if acc' >= p 
                then a
                else walk acc' p r
