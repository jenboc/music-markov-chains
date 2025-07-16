module Music.Canonical
    (
        canonicalForm,
        SemanticEq(..)
    ) where

import Music.Types (Music)
import Music.Conversion (midiToMusic, musicToMidi)

canonicalForm :: Music -> Music
canonicalForm = snd . midiToMusic . musicToMidi tpq
    where tpq = 2

-- Allow for semantic equality of music
-- Means we can leave the std. eq (==) to compare structure
class SemanticEq a where
    (===) :: a -> a -> Bool

instance SemanticEq Music where
    a === b = canonicalForm a == canonicalForm b
