module Music.Types 
    (
        PitchClass(..),
        Pitch(..),
        Duration(..),
        Note(..),
        Music(..),
        Transposable(..),
        sequentialise,
        parallelise,
        intToPitch,
        pitchToInt,
        expandRepeat
    ) where

import Data.Maybe (fromMaybe)

type Octave = Int

data PitchClass = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B 
    deriving (Enum, Eq, Show, Ord)

data Pitch = Pitch PitchClass Octave
    deriving (Eq, Show)

data Duration = Sixteenth | Eighth | Quarter | Half | Whole
    deriving (Eq, Show, Ord)

data Note = Note Pitch Duration | Rest Duration
    deriving (Eq, Show)

data Music = Single Note
           | Sequential Music Music
           | Parallel Music Music
           | Repeat Int Music
    deriving Show

expandRepeat :: Music -> Music
expandRepeat (Repeat n m) = fromMaybe (Single $ Rest Sixteenth) 
    $ sequentialise $ replicate n m
expandRepeat m = m

class Transposable a where
    transpose :: Int -> a -> a

instance Transposable Pitch where
    transpose semis = intToPitch . (+ semis) . pitchToInt

instance Transposable Note where
    transpose _ (Rest d) = Rest d
    transpose semis (Note p d) = Note (transpose semis p) d

instance Transposable Music where
    transpose semis (Single n) = Single (transpose semis n)
    transpose semis (Sequential m1 m2) = 
        Sequential (transpose semis m1) (transpose semis m2)
    transpose semis (Parallel m1 m2) = 
        Parallel (transpose semis m1) (transpose semis m2)
    transpose semis (Repeat n m) = Repeat n (transpose semis m)

joinMusic :: (Music -> Music -> Music) -> [Music] -> Maybe Music
joinMusic _ [] = Nothing
joinMusic _ [m] = Just m
joinMusic f (m:ms) = joinMusic f ms >>= Just . f m

sequentialise :: [Music] -> Maybe Music
sequentialise = joinMusic Sequential

parallelise :: [Music] -> Maybe Music
parallelise = joinMusic Parallel

pitchToInt :: Pitch -> Int
pitchToInt (Pitch pc oct) = 12 * (oct + 1) + fromEnum pc

intToPitch :: Int -> Pitch
intToPitch x = let o = (x `div` 12) - 1
                   pc = x `mod` 12
               in Pitch (toEnum pc) o

