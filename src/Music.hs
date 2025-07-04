module Music 
    (
        PitchClass(..),
        Pitch(..), 
        Duration(..), 
        Note(..), 
        Music(..),
        Scale(..),
        Transposable(..),
        majorScale,
        minorScale,
        musicToMidi
    ) where

import Data.List (sortOn)
import Codec.Midi

-- Music ADTs
type Octave = Int
type MidiTrack = [(Int, Message)]

data PitchClass = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B 
    deriving (Enum, Eq, Show)

data Pitch = Pitch PitchClass Octave
    deriving (Eq, Show)

data Duration = Whole | Half | Quarter | Eighth | Sixteenth
    deriving (Eq, Show)

data Note = Note Pitch Duration | Rest Duration
    deriving Show

data Music = Single Note
           | Sequential Music Music
           | Parallel Music Music
           | Repeat Int Music
    deriving Show

-- Transposition
newtype Scale = Scale [Pitch]

instance Show Scale where
    show (Scale ps) = show ps

class Transposable a where
    transpose :: Int -> a -> a

pitchToInt :: Pitch -> Int
pitchToInt (Pitch pc oct) = 12 * (oct + 1) + fromEnum pc

intToPitch :: Int -> Pitch
intToPitch x = let o = (x `div` 12) - 1
                   pc = x `mod` 12
               in Pitch (toEnum pc) o

instance Transposable Pitch where
    transpose semis = intToPitch . (+ semis) . pitchToInt

instance Transposable Note where
    transpose semis (Rest d) = Rest d
    transpose semis (Note p d) = Note (transpose semis p) d

instance Transposable Music where
    transpose semis (Single n) = Single (transpose semis n)
    transpose semis (Sequential m1 m2) = 
        Sequential (transpose semis m1) (transpose semis m2)
    transpose semis (Parallel m1 m2) = 
        Parallel (transpose semis m1) (transpose semis m2)
    transpose semis (Repeat n m) = Repeat n (transpose semis m)

instance Transposable Scale where
    transpose semis (Scale ps) = Scale $ map (transpose semis) ps

majorScale :: PitchClass -> Scale
majorScale root = Scale $ map ($ Pitch root 0) transpositions
    where
        transpositions = 
            [
                id,
                transpose 2,
                transpose 4,
                transpose 5,
                transpose 7,
                transpose 9,
                transpose 11
            ]

minorScale :: PitchClass -> Scale
minorScale root = Scale $ map ($ Pitch root 0) transpositions
    where
        transpositions = 
            [
                id,
                transpose 2,
                transpose 3,
                transpose 5,
                transpose 7,
                transpose 8,
                transpose 10
            ]

-- Music to MIDI
durationToTicks :: Int -> Duration -> Int
durationToTicks tsPerQuarter dur
    | dur == Whole = 4 * tsPerQuarter
    | dur == Half = 2 * tsPerQuarter
    | dur == Quarter = tsPerQuarter
    | dur == Eighth = tsPerQuarter `div` 2
    | otherwise = tsPerQuarter `div` 4

totalDuration :: Int -> Music -> Int
totalDuration tsPerQuarter (Single (Note _ d)) = durationToTicks tsPerQuarter d
totalDuration tsPerQuarter (Single (Rest d)) = durationToTicks tsPerQuarter d
totalDuration tsPerQuarter (Sequential m1 m2) = 
    totalDuration tsPerQuarter m1 + totalDuration tsPerQuarter m2
totalDuration tsPerQuarter (Parallel m1 m2) =
    max (totalDuration tsPerQuarter m1) (totalDuration tsPerQuarter m2)
totalDuration tsPerQuarter (Repeat n m) = n * totalDuration tsPerQuarter m

musicToTrack :: Int -> Music -> MidiTrack
musicToTrack tsPerQuarter = toDeltaTime 0 . sortOn fst . walk 0
    where
        -- Partially apply util functions with ticksPerQuarter
        durToTicks :: Duration -> Int
        durToTicks = durationToTicks tsPerQuarter
        
        totalDur :: Music -> Int
        totalDur = totalDuration tsPerQuarter
        
        -- Flatten the Music structure into a list of absolutely timed
        -- events (for context: MIDI expects delta time)
        walk :: Int -> Music -> [(Int, Message)]
        walk t (Single (Note p d)) = 
            [
                (t, NoteOn 0 (pitchToInt p) 100),
                (t + durToTicks d, NoteOff 0 (pitchToInt p) 100)
            ]
        walk _ (Single (Rest _)) = []
        walk t (Sequential m1 m2) = let w1 = walk t m1
                                        w2 = walk (t + totalDur m1) m2
                                    in w1 ++ w2
        walk t (Parallel m1 m2) = [m1, m2] >>= walk t
        walk t (Repeat n m) = let d = totalDur m in concat 
            [walk (t + i * d) m | i <- [0..n-1]]

        -- Transform the (sorted) absolutely timed events into delta timed ones
        -- (Also sneakily add in the TrackEnd event)
        toDeltaTime :: Int -> [(Int, Message)] -> MidiTrack
        toDeltaTime _ [] = [(0, TrackEnd)]
        toDeltaTime prev ((t,m):xs) = (t - prev, m) : toDeltaTime t xs

musicToMidi :: Int -> Music -> Midi
musicToMidi tsPerQuarter mus = Midi
    {
        fileType = MultiTrack,
        timeDiv = TicksPerBeat tsPerQuarter,
        tracks = [musicToTrack tsPerQuarter mus]
    }
