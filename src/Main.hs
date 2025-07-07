module Main where

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import qualified Data.Map as M

import Music
import Generation
import Codec.Midi
import System.Random
import Data.Functor
import Data.List (sort, group)

notePattern :: Pitch -> Music
notePattern (Pitch pc o) = case sequentialise ns of
    Nothing -> Single (Rest Whole)
    Just m -> m
    where
        n :: Duration -> Music
        n d = Single $ Note (Pitch pc o) d
        r = Single $ Rest Sixteenth
        ns = map n [Quarter, Eighth] ++ [r, n Eighth, r] ++ map n [Eighth, Quarter]

mapPattern :: Music -> Music
mapPattern (Single (Rest r)) = Single $ Rest r
mapPattern (Single (Note p _)) = notePattern p
mapPattern (Sequential m1 m2) = Sequential (mapPattern m1) (mapPattern m2)
mapPattern (Parallel m1 m2) = Parallel (mapPattern m1) (mapPattern m2)
mapPattern (Repeat n m) = Repeat n $ mapPattern m

type Distribution a = [(a, Double)]

uniformScaleDist :: Int -> PitchClass -> (Distribution PitchClass, Distribution Int)
uniformScaleDist o pc = (pcDist, oDist)
    where
        scale :: ([PitchClass], [Int])
        scale = let (Scale pits) = majorScale pc in
            unzip $ map (\(Pitch cls oct) -> (cls, oct)) 
                $ map (transpose (12 * o)) pits

        pcDist :: Distribution PitchClass
        pcDist = zip (fst scale) $ replicate (length $ fst scale) (1/7)

        oDist :: Distribution Int
        oDist = map (\grp -> (head grp, fromIntegral (length grp) / fromIntegral (length (snd scale)))) 
            $ group $ sort $ snd scale

ps :: Probabilities
ps = Probabilities
    {
        musicProbs = MusicProbs
            {
                repeated = 0,
                single = 0.2,
                sequential = 0.4,
                parallel = 0.4
            },
        noteProbs = NoteProbs
            {
                note = 0.8,
                rest = 0.2,
                duration =
                    [
                        (Whole, 0.1),
                        (Half, 0.4),
                        (Quarter, 0.5)
                    ],
                pitchClass = pcDist,
                octave = 
                    [
                        (2, 0.1),
                        (3, 0.2),
                        (4, 0.5),
                        (5, 0.2)
                    ]
            }
    }
    where
        (pcDist, oDist) = uniformScaleDist 4 G

dble :: Int -> Double
dble n = fromIntegral n :: Double

main :: IO ()
main = do
    putStrLn "Letsa go"
    nStr <- getLine
    let n = read nStr :: Int
    
    putStrLn $ "Generating Music Tree of Height " ++ nStr
    m <- randomGen ps n
    print m
    exportFile "generated.mid" $ musicToMidi 480 m
