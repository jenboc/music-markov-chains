module Main where

import Music
import Generation
import Codec.Midi
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

uniformScaleDist :: Int -> Scale -> (Distribution PitchClass, Distribution Int)
uniformScaleDist o (Scale pits) = (pcDist, oDist)
    where
        scale :: ([PitchClass], [Int])
        scale = unzip $ map (\(Pitch cls oct) -> (cls, oct)) 
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
        (pcDist, oDist) = uniformScaleDist 4 (majorScale G)

cMajorChord :: Int -> Duration -> Music
cMajorChord = getScaleChord (majorScale G) 4

bayesNet :: BayesNet Music
bayesNet = cChordNet
    where
        cChordNet = Node (cMajorChord 0 Half)
            [
                (fChordNet, 0.3),
                (gChordNet, 0.3),
                (dChordNet, 0.2),
                (eChordNet, 0.2)
            ]
        fChordNet = Node (cMajorChord 3 Half)
            [
                (cChordNet, 0.3),
                (gChordNet, 0.3),
                (dChordNet, 0.2),
                (eChordNet, 0.2)
            ]
        gChordNet = Node (cMajorChord 4 Half)
            [
                (fChordNet, 0.3),
                (cChordNet, 0.3),
                (dChordNet, 0.2),
                (eChordNet, 0.2)
            ]
        dChordNet = Node (cMajorChord 1 Half)
            [
                (fChordNet, 0.1),
                (gChordNet, 0.1),
                (cChordNet, 0.3),
                (eChordNet, 0.5)
            ]
        eChordNet = Node (cMajorChord 2 Half)
            [
                (fChordNet, 0.3),
                (gChordNet, 0.1),
                (dChordNet, 0.5),
                (cChordNet, 0.1)
            ]

dble :: Int -> Double
dble n = fromIntegral n :: Double

main :: IO ()
main = do
    putStrLn "Letsa go"
    
    m <- bayesGen bayesNet 30
    print m
    exportFile "generated.mid" $ musicToMidi 480 (mapPattern m)
