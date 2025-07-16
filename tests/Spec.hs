import Test.Tasty
import Test.Tasty.HUnit

import Music.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Music Mock Tests"
    [
        testCase "different octaves not equal" 
            $ Pitch C 4 /= Pitch C 5 @? "Pitch C 4 should not equal Pitch C 5"
    ]
