module Main where

import Music
import Codec.Midi

wholeNote :: Int -> PitchClass -> Music
wholeNote o pc = case sequentialise ns of
    Nothing -> Single (Rest Whole)
    Just m -> m
    where
        n :: Duration -> Music
        n d = Single $ Note (Pitch pc o) d
        r = Single $ Rest Sixteenth
        ns = map n [Quarter, Eighth] ++ [r, n Eighth, r] ++ map n [Eighth, Quarter]

c :: Music
c = Single (Note (Pitch C 4) Whole)

e :: Music
e = Single (Note (Pitch E 4) Whole)

g :: Music
g = Single (Note (Pitch E 4) Whole)

parallelise :: [Music] -> Maybe Music
parallelise [] = Nothing
parallelise [m] = Just m
parallelise (m:ms) = parallelise ms >>= Just . Parallel m

sequentialise :: [Music] -> Maybe Music
sequentialise [] = Nothing
sequentialise [m] = Just m
sequentialise (m:ms) = sequentialise ms >>= Just . Sequential m

chord :: Int -> [PitchClass] -> Music
chord o pcs = case parallelise $ map (wholeNote o) pcs of
    Just m -> m
    Nothing -> Single (Rest Whole)

chords :: [Music]
chords = map (uncurry chord)
    [
        (4, [C, E, G]),
        (4, [G, B, D]),
        (4, [F, A, C]),
        (4, [C, E, G]),
        (4, [C, E, G]),
        (4, [F, A, C]),
        (4, [G, B, D]),
        (5, [C, E, G]),
        (4, [F, A, C])
    ]

myMusic :: Music
myMusic = case sequentialise chords of
    Just m -> Repeat 1 m
    Nothing -> Single (Rest Whole)

main :: IO ()
main = do
    putStrLn "Exporting Midi File"
    exportFile "exported-midi.mid" $ musicToMidi 480 myMusic
    putStrLn "Midi Exported!"
