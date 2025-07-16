module Music.CanonicalTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Music.Types
import Music.Canonical

tests :: TestTree
tests = testGroup "Music.Canonical"
    [
        testCase "canonicalForm separates chords"
            $ canonicalForm testChordInput @?= testChordExpected,

        testCase "canonicalForm does not impact canonical form"
            $ canonicalForm (canonicalForm complexMusic) @?= canonicalForm complexMusic,

        testCase "canonicalForm a == canonicalForm b => a === b"
            $ testChordInput === testChordExpected @? "=== failed"
    ]

-- Simplified Note constructor
note :: Duration -> Music
note = Single . Note (Pitch C 4)

testChordInput :: Music
testChordInput = Parallel (Sequential (note Whole) (note Whole))
        (Parallel (Sequential (note Whole) (note Whole)) (Sequential (note Whole) (note Whole)))

testChordExpected :: Music
testChordExpected = Sequential
    (Parallel (note Whole) (Parallel (note Whole) (note Whole)))
    (Parallel (note Whole) (Parallel (note Whole) (note Whole)))

complexMusic :: Music
complexMusic =
  Repeat 2 $
    Sequential
      (Parallel
        (Single (Note (Pitch C 4) Quarter))
        (Single (Rest Eighth)))
      (Sequential
        (Single (Note (Pitch G 4) Eighth))
        (Repeat 3
          (Parallel
            (Single (Note (Pitch E 4) Sixteenth))
            (Single (Note (Pitch B 3) Sixteenth)))))
