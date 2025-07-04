module Music.Conversion
    (
        musicToMidi,
        midiToMusic
    ) where

import Music.Types

import qualified Data.Map as M
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Codec.Midi

type MidiTrack = [(Int, Message)]

-- ###### Music -> MIDI ######
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

-- ####### MIDI -> Music #######
trackToMusic :: Int -> MidiTrack -> Music
trackToMusic tsPerQuarter = assemble 0 . sortOn (\(x,_,_) -> x) . extractNotes M.empty [] . toAbsTime
    where
        toAbsTime :: MidiTrack -> MidiTrack
        toAbsTime = snd . foldl walkTrack (0, []) . sortOn fst

        walkTrack :: (Int, MidiTrack) -> (Int, Message) -> (Int, MidiTrack)
        walkTrack (accTime, accTrack) (dt, msg) = let t' = accTime + dt
            in (t', accTrack ++ [(t', msg)])

        extractNotes :: M.Map (Int,Int) Int -> [(Int, Int, Note)] -> MidiTrack -> [(Int, Int, Note)]
        extractNotes _ acc [] = reverse acc
        extractNotes active acc ((t,m):r) = case m of
            NoteOn c p v | v > 0 -> 
                extractNotes (M.insert (c,p) t active) acc r

            NoteOff c p _ -> case M.lookup (c,p) active of
                Nothing -> extractNotes active acc r
                Just tOn -> let p' = intToPitch p
                                dur = ticksToDuration tsPerQuarter (t - tOn)
                    in extractNotes (M.delete (c,p) active) 
                        ((tOn, t, Note p' dur) : acc) r

            -- MIDI Std: NoteOn w/ v = 0 === NoteOff
            NoteOn c p v | v <= 0 ->
                extractNotes active acc ((t, NoteOff c p v):r)

            -- Anything else - just ignore it
            _ -> extractNotes active acc r

        assemble :: Int -> [(Int, Int, Note)] -> Music
        assemble _ [] = Single (Rest Quarter)
        assemble t ((s,e,n):r)
            | t < s = let restDur = ticksToDuration tsPerQuarter (s - t)
                      in Sequential (Single (Rest restDur)) 
                        (Sequential (Single n) (assemble e r))
            | otherwise = Sequential (Single n) (assemble e r)

midiToMusic :: Midi -> (Int, Music)
midiToMusic midi = let tpq = calculateTicksPerQuarter $ timeDiv midi
                       mus = parallelise $ map (trackToMusic tpq) $ tracks midi
                    in (tpq, fromMaybe (Single (Rest Whole)) mus)

-- ###### HELPER FUNCTIONS ######
ticksToDuration :: Int -> Int -> Duration
ticksToDuration tsPerQuarter ts
    | ts >= 4 * tsPerQuarter = Whole
    | ts >= 2 * tsPerQuarter = Half
    | ts >= tsPerQuarter = Quarter
    | ts >= tsPerQuarter `div` 2 = Eighth
    | otherwise = Sixteenth

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

calculateTicksPerQuarter :: TimeDiv -> Int
calculateTicksPerQuarter (TicksPerBeat ts) = ts
calculateTicksPerQuarter (TicksPerSecond _ _) = error "Not implemented"
