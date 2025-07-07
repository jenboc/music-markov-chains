module Generation
    (
        MusicProbs(..),
        NoteProbs(..),
        Probabilities(..),
        randomGen
    ) where

import Music
import System.Random (randomRIO)
import qualified Data.Map as M
import Data.Functor

data MusicProbs = MusicProbs
    {
        single :: Double,
        sequential :: Double,
        parallel :: Double,
        repeated :: Double
    }

data NoteProbs = NoteProbs
    {
        note :: Double,
        rest :: Double,
        duration :: [(Duration, Double)],
        pitchClass :: [(PitchClass, Double)],
        octave :: [(Int, Double)]
    }

data Probabilities = Probabilities
    {
        musicProbs :: MusicProbs,
        noteProbs :: NoteProbs
    }

-- Music object which we will use as "empty" (since the library doesn't have
-- one - and it seems stupid to wrap it to include it)
nullMusic :: Music
nullMusic = Single $ Rest Sixteenth


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

randomNum :: IO Int
randomNum = randomRIO (1,6)

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
            a <- randomNum
            b <- randomGen ps (n-1)
            return $ Repeat a b
