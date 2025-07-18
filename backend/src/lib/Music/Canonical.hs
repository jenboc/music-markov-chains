{-
    Due to the way I chose to represent music, there are multiple ways to
    describe the same musical structures (e.g. sequential chords). We
    need some kind of "canonial form" so we can see if two music trees
    represent the same thing

    The canonical form minimises the amount of nodes in a parallel structure,
    i.e. s.t. a parallel structure only represents an individual chord
-}

module Music.Canonical
    (
        canonicalForm,
        SemanticEq(..)
    ) where

import Music.Types (Music(..), Note(..), Duration(..), expandRepeat, sequentialise, parallelise)
import Music.Conversion (midiToMusic, musicToMidi, totalDuration, ticksToDuration, durationToTicks)
import Data.Maybe (fromMaybe, catMaybes)

-- Set a constant ticks per quarter to use when converting to canonical form
tpq :: Int
tpq = 2

-- Convert a music structure to canonical form
canonicalForm :: Music -> Music
canonicalForm s@(Single _) = s
canonicalForm r@(Repeat _ _) = canonicalForm $ expandRepeat r
canonicalForm (Sequential a b) = Sequential (canonicalForm a) (canonicalForm b)
canonicalForm (Parallel a b) =
    let p' = Parallel (canonicalForm a) (canonicalForm b)
        flattened = flattenParallel p'
        maxFirstNoteTicks = maximum $ map (totalDuration tpq . firstNote) flattened
        (toParallel, extras) = unzip $ map (partitionOnTicks maxFirstNoteTicks) flattened
        toParallel' = filter (not . isRest) toParallel
        extras' = parallelise $ filter (not . isRest) $ catMaybes extras
    in fromMaybe (Single $ Rest Whole) $ case extras' of 
        Just remaining -> do
            thisParallel <- parallelise toParallel'
            sequentialise [thisParallel, remaining]
        Nothing -> parallelise toParallel'

isRest :: Music -> Bool
isRest (Single (Rest _)) = True
isRest (Sequential a b) = isRest a && isRest b
isRest (Parallel a b) = isRest a && isRest b
isRest m = False

-- Get a list of everything in a parallel block
flattenParallel :: Music -> [Music]
flattenParallel (Parallel a b) = flattenParallel a ++ flattenParallel b
flattenParallel m = [m]

-- Get the first element in a sequential
firstNote :: Music -> Music
firstNote (Sequential a _) = a
firstNote m = m

partitionOnTicks :: Int -> Music -> (Music, Maybe Music)
partitionOnTicks ts r@(Repeat _ _) = partitionOnTicks ts $ expandRepeat r
partitionOnTicks ts (Sequential a b)
    | aTicks >= ts = (a, Just b)
    | otherwise = 
        let (b', extra) = partitionOnTicks ts' b
        in (Sequential a b', extra)
    where
        aTicks = totalDuration tpq a
        ts' = ts - aTicks
partitionOnTicks _ m = (m, Nothing)

takeTicks :: Int -> Music -> Music
takeTicks d r@(Repeat _ _) = takeTicks d $ expandRepeat r
takeTicks dTicks (Sequential a b)
    | aTicks >= dTicks = a
    | otherwise = Sequential a (takeTicks dTicks' b)
    where
        aTicks = totalDuration tpq a
        dTicks' = dTicks - aTicks
takeTicks _ m = m

-- Extend a piece of music to a certain length
extendLengthTo :: Int -> Music -> Music
extendLengthTo ticks m = 
    let durDiff = ticks - totalDuration tpq m
        restSeq = restForTicks ticks
    in if durDiff <= 0 
        then m 
        else Sequential m restSeq

restForTicks :: Int -> Music
restForTicks ts = fromMaybe (Single $ Rest Whole) $ sequentialise $ go ts
    where
        go :: Int -> [Music]
        go ts
            | ts <= 0 = []
            | otherwise = Single (Rest dur) : go ts'
            where
                dur = ticksToDuration tpq ts
                ts' = ts - durationToTicks tpq dur

-- Allow for semantic equality of music
-- Means we can leave the std. eq (==) to compare structure
class SemanticEq a where
    (===) :: a -> a -> Bool

instance SemanticEq Music where
    a === b = canonicalForm a == canonicalForm b
