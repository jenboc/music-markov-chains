module Music (Pitch(..), Duration(..), Note(..), Music(..)) where

-- Music ADTs
data Pitch = A | As | B | C | Cs | D | Ds | E | F | Fs | G | Gs
    deriving (Enum, Eq, Show)

data Duration = Whole | Half | Quarter | Eighth | Sixteenth
    deriving (Eq, Show)

data Note = Note Pitch Duration | Rest Duration

data Music = Single Note
           | Sequential Music Music
           | Parallel Music Music
           | Repeated Music Music
           | Transform (Note -> Note) Music
