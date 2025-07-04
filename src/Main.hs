module Main where

import Music
import Codec.Midi

notePattern :: Int -> PitchClass -> Music
notePattern o pc = case sequentialise ns of
    Nothing -> Single (Rest Whole)
    Just m -> m
    where
        n :: Duration -> Music
        n d = Single $ Note (Pitch pc o) d
        r = Single $ Rest Sixteenth
        ns = map n [Quarter, Eighth] ++ [r, n Eighth, r] ++ map n [Eighth, Quarter]

parallelise :: [Music] -> Maybe Music
parallelise [] = Nothing
parallelise [m] = Just m
parallelise (m:ms) = parallelise ms >>= Just . Parallel m

sequentialise :: [Music] -> Maybe Music
sequentialise [] = Nothing
sequentialise [m] = Just m
sequentialise (m:ms) = sequentialise ms >>= Just . Sequential m

playScale :: Scale -> Int -> Duration -> Music
playScale (Scale ps) o d = case sequentialise notes of   
    Just m -> m
    Nothing -> Single (Rest Whole)
    where
        notes = map (\(Pitch pc o') -> Single $ Note (Pitch pc (o + o')) d) ps

justSequentialise :: [Music] -> Music
justSequentialise ms = case sequentialise ms of
    Just m -> m
    Nothing -> Single (Rest Whole)

justParallelise :: [Music] -> Music
justParallelise ms = case parallelise ms of
    Just m -> m
    Nothing -> Single (Rest Whole)

pitchFrom :: Scale -> Int -> Int -> Pitch
pitchFrom (Scale ps) a n = let a' = a `mod` length ps
                               b = (a' + n) `mod` length ps
                               dOct = (a' + n) `div` length ps
                           in (\(Pitch pc o) -> Pitch pc (o + dOct)) $ ps !! b

scaleChord :: Scale -> Int -> Int -> Duration -> Music
scaleChord s oct n d = transpose (12 * oct) $ justParallelise 
    $ map ((\p -> Single (Note p d)) . pitchFrom s n) [0, 2, 4]

chordProgression :: Scale -> Int -> Duration -> [Int] -> Music
chordProgression s oct d = justSequentialise . map (\x -> scaleChord s oct x d)

exportMusic :: Music -> IO ()
exportMusic mus = do
    putStrLn "Exporting Music as Midi File"
    exportFile "exported-midi.mid" $ musicToMidi 480 mus
    putStrLn "Music Exported!"

main :: IO ()
main = do
    let g = chordProgression (majorScale G) 4 Half [1, 5, 4]
        g' = justParallelise [g, transpose (-12) g]

    exportMusic $ justSequentialise [g', Single $ Rest Whole, g']
