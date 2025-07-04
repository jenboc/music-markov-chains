module Main where

import Codec.Midi

track0 :: [(Int, Message)]
track0 = 
    [
        (0, NoteOn 0 60 80),
        (24, NoteOff 0 60 0),
        (0, TrackEnd)
    ]

track1 :: [(Int, Message)]
track1 =
    [
        (0, NoteOn 0 64 80),
        (24, NoteOn 0 64 0),
        (0, TrackEnd)
    ]

midiFile :: Midi
midiFile = Midi
    {
        fileType = MultiTrack,
        timeDiv = TicksPerBeat 24,
        tracks = [track0, track1]
    }

main :: IO ()
main = do
    putStrLn "Exporting Midi File"
    exportFile "exported-midi.mid" midiFile
    putStrLn "Midi Exported!"
