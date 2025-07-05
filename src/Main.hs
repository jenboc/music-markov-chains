module Main where

import Data.Maybe (fromMaybe)
import Music.Types
import Music.Composition
import Music.Conversion
import Codec.Midi

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

exportMusic :: FilePath -> Music -> IO ()
exportMusic name mus = do
    putStrLn "Exporting Music as Midi File"
    exportFile name $ musicToMidi 480 mus
    putStrLn "Music Exported!"

main :: IO ()
main = do
    let scale = majorScale Ds
        getPitch x = transpose 36 . Single . flip Note Whole . getScalePitchFrom scale x
    let original = mapPattern $ Sequential
            (chordProgression scale 3 Whole [2,3,6])  
            (fromMaybe (Single $ Rest Whole) $ parallelise $ (\l -> head l : transpose (-1) (l !! 1) : [last l]) $ map (getPitch 5) [0, 2, 4])

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
