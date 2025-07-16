import Test.Tasty
import Test.Tasty.HUnit

import qualified Music.CanonicalTest

import Music.Types


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Music Mock Tests"
    [
        Music.CanonicalTest.tests
    ]
