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

exportMusic :: FilePath -> Music -> IO ()
exportMusic name mus = do
    putStrLn "Exporting Music as Midi File"
    exportFile name $ musicToMidi 480 mus
    putStrLn "Music Exported!"

main :: IO ()
main = do
    let original = chordProgression (majorScale Ds) 3 Half [2,3,6]

    putStrLn "Creating Original Music"
    exportMusic "original.mid" original
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
