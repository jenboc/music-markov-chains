module Main where

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

import Music
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

yippee :: Midi -> IO ()
yippee midi = do
    let mus = snd $ midiToMusic midi
    print mus
    putStrLn "Reexporting music"
    exportMusic "reexported.mid" mus

main :: IO ()
main = do
    args <- getArgs

    let filepath = head args

    res <- importFile filepath
    case res of
        Left str -> putStrLn str
        Right midi -> yippee midi
