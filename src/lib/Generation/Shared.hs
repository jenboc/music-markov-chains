module Generation.Shared
    (
        MusicProbs(..),
        NoteProbs(..),
        Probabilities(..),
        chooseFromProbList,
        randomMusic,
        randomNote
    ) where

import Music
import Data.Functor
import System.Random (randomRIO)

-- Probability Tables
data MusicProbs = MusicProbs
    {
        single :: Double,
        sequential :: Double,
        parallel :: Double,
        repeated :: Double
    }
    deriving (Show, Eq)

data NoteProbs = NoteProbs
    {
        note :: Double,
        rest :: Double,
        duration :: [(Duration, Double)],
        pitchClass :: [(PitchClass, Double)],
        octave :: [(Int, Double)]
    }
    deriving (Show, Eq)

data Probabilities = Probabilities
    {
        musicProbs :: MusicProbs,
        noteProbs :: NoteProbs
    }
    deriving (Show, Eq)

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

-- Choose a random Music construct
randomMusic :: MusicProbs -> IO Music
randomMusic ps = chooseFromProbList l
    where
        l =
            [
                (nullMusic, single ps),
                (Sequential nullMusic nullMusic, sequential ps),
                (Parallel nullMusic nullMusic, parallel ps),
                (Repeat 0 nullMusic, repeated ps)
            ]

-- Choose a random note
randomNote :: NoteProbs -> IO Note
randomNote ps = do
    d <- chooseFromProbList $ duration ps
    n <- chooseFromProbList noteList
    case n of
        (Note _ _) -> do
            pc <- chooseFromProbList $ pitchClass ps
            o <- chooseFromProbList $ octave ps
            let p = Pitch pc o
            return $ Note p d
        (Rest _) -> return $ Rest d
    where
        noteList = 
            [
                (Note (Pitch C 0) Sixteenth, note ps),
                (Rest Sixteenth, rest ps)
            ]

