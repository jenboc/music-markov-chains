module Generation.Random
    (
        randomGen,
        randomProbabilities,
        randomNoteProbs,
        randomMusicProbs
    ) where

import Generation.Shared
import Music
import Data.Functor
import System.Random (randomRIO)

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
