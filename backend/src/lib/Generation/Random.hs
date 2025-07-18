{-
    Music Generation based off of flat probabilities
    No Markov Chains here! 
-}

module Generation.Random
    (
        MusicProbs(..),
        NoteProbs(..),
        Probabilities(..),
        randomGen,
        randomProbabilities,
        randomNoteProbs,
        randomMusicProbs
    ) where

import Generation.Shared
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

-- Create a list of n numbers which sums to 1
randomProbList :: Int -> IO [Double]
randomProbList = randomProbList' 1
    where
        randomProbList' :: Double -> Int -> IO [Double]
        randomProbList' _ 0 = return []
        randomProbList' r 1 = return [r]
        randomProbList' r n = do
            num <- randomRIO (0,r)
            others <- randomProbList' (r - num) (n - 1)
            return $ num : others

-- Generate a random music table
randomMusicProbs :: IO MusicProbs
randomMusicProbs = do
    (sing:seq:par:rep:r) <- randomProbList 4
    return $ MusicProbs
        {
            single = sing,
            sequential = seq,
            parallel = par,
            repeated = rep
        }

-- Generate a random note table
randomNoteProbs :: IO NoteProbs
randomNoteProbs = do
    (noteProb:restProb:r) <- randomProbList 2
    durationProbs <- randomProbList 5
    pitchClassProbs <- randomProbList 12
    octaveProbs <- randomProbList 3
    return $ NoteProbs
        {
            note = noteProb,
            rest = restProb,
            duration = zip [Whole, Half, Quarter, Eighth, Sixteenth]
                durationProbs,
            pitchClass = zip [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
                pitchClassProbs,
            octave = zip [3, 4, 5] octaveProbs
        }

-- Generate a full probability table
randomProbabilities :: IO Probabilities
randomProbabilities = do
    music <- randomMusicProbs
    notes <- randomNoteProbs
    return $ Probabilities
        {
            musicProbs = music,
            noteProbs = notes
        }
    
randomGen :: Probabilities -> Int -> IO Music
randomGen ps 0 = randomNote (noteProbs ps) <&> Single
randomGen ps n = do
    m <- randomMusic (musicProbs ps) 
    case m of
        (Single _) -> do
            a <- randomNote (noteProbs ps)
            return $ Single a
        (Sequential _ _) -> do
            a <- randomGen ps (n-1)
            b <- randomGen ps (n-1)
            return $ Sequential a b
        (Parallel _ _) -> do
            a <- randomGen ps (n-1)
            b <- randomGen ps (n-1)
            return $ Parallel a b
        (Repeat _ _) -> do
            a <- randomRIO (1,6)
            b <- randomGen ps (n-1)
            return $ Repeat a b
