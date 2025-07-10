module Main where

import Music
import Generation
import Codec.Midi
import System.Environment (getArgs)

importMusic :: String -> IO Music
importMusic filename = do
    res <- importFile filename
    case res of
        Left str -> do
            putStrLn $ "Failed to import file " ++ str
            return $ Single $ Rest Whole
        Right midi -> return $ snd $ midiToMusic midi

main :: IO ()
main = do
    (filename:nStr:lStr:_) <- getArgs

    putStr $ "Constructing an " ++ nStr ++ "-gram model of " ++ filename
    musicFile <- importMusic filename
    let n = read nStr :: Int
        l = read lStr :: Int
        bayesNet = createBayesMusicNet n musicFile

    putStrLn $ "Generating a " ++ lStr ++ " note length piece based on this model"
    bayesRand <- bayesGen bayesNet l

    putStrLn "Exporting this piece as ./bayes.mid"
    exportFile "bayes.mid" $ musicToMidi 480 bayesRand
