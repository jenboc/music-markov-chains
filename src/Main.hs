module Main where

import Music.Types
import Music.Composition
import Music.Conversion
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

filename :: String
filename = "exported-midi.mid"

exportMusic :: FilePath -> Music -> IO ()
exportMusic name mus = do
    putStrLn "Exporting Music as Midi File"
    exportFile name $ musicToMidi 480 mus
    putStrLn "Music Exported!"

main :: IO ()
main = do
    let original = chordProgression (majorScale C) 3 Half [1,4,5]

    putStrLn "Creating Original Music"
    exportMusic "original.mid" $ original
    putStrLn "Original Exported"

    res <- importFile "original.mid"
    case res of
        Left str -> putStrLn $ "Problem importing file: " ++ str
        Right midi -> do
            let (tpq, mus) = midiToMusic midi
            putStrLn $ "Imported Music @ " ++ show tpq ++ " ticks per quarter"
            exportMusic "reexported.mid" mus
            putStrLn "Re-exported Music"

            print original
            putStrLn "\n\n\n"
            print mus
