module Main where

import Music
import Generation
import Codec.Midi
import System.Environment (getArgs)
import System.Directory (listDirectory)
import Data.Maybe
import Data.List
import qualified Data.Map as M

importMusic :: String -> IO Music
importMusic filename = do
    res <- importFile filename
    case res of
        Left str -> do
            putStrLn $ "Failed to import file " ++ str
            return $ Single $ Rest Whole
        Right midi -> return $ snd $ midiToMusic midi

graphStats :: Graph a b -> IO ()
graphStats g = do
    let labels = M.keys (labelToDataMap g)
    
    putStrLn ""
    putStrLn $ "Graph has " ++ show (length labels) ++ " labels"
    putStrLn $ intercalate ", " $ map show labels

    putStrLn $ intercalate "\n" $ map (\(k,v) -> show k ++ " has " ++ show (M.size v) ++ " connections") (M.toList $ adjList g)

mostConnectedLabel :: Graph a b -> (Int,Int)
mostConnectedLabel = 
    foldl findMax (0,0) . map (\(k,v) -> (k, M.size v)) . M.toList . adjList
    where
        findMax m@(_,maxCount) n@(_,count)
            | count > maxCount = n
            | otherwise = m

exportMusic :: String -> Music -> IO ()
exportMusic fname m = exportFile fname $ musicToMidi 480 m

endsWith :: String -> String -> Bool
endsWith str suffix = reverse (take (length suffix) $ reverse str) == suffix

height :: Music -> Int
height (Parallel a b) = height a + height b
height (Sequential a b) = max (height a) (height b)
height r@(Repeat _ _) = height $ expandRepeat r
height _ = 1

mlength :: Music -> Int
mlength (Sequential a b) = mlength a + mlength b
mlength (Parallel a b) = max (mlength a) (mlength b)
mlength r@(Repeat _ _) = mlength $ expandRepeat r
mlength _ = 1

complexStats :: ComplexModel -> IO ()
complexStats (ComplexModel dChain pChain) = graphStats dChain >> graphStats pChain

main :: IO ()
main = do
    (fname:_) <- getArgs

    music <- importMusic fname
    
    let music' = canonicalForm music
        flattened = flattenSequentials music' >>= flattenParallels
        durations = mapMaybe getDuration flattened
        pitches = mapMaybe getPitch flattened
    
    putStrLn "num durations then num pitches"
    print $ length $ nub durations
    print $ length $ nub pitches
    genTest music
    where
        -- Get the duration of a single note
        getDuration :: Music -> Maybe Duration
        getDuration (Single n) = case n of
            Note _ d -> Just d
            Rest d -> Just d
        getDuration _ = Nothing

        -- Get pitch of a single note
        getPitch :: Music -> Maybe Pitch
        getPitch (Single (Note p _)) = Just p
        getPitch _ = Nothing

genTest :: Music -> IO ()
genTest music = do
    let seqFlatNaive = createNaiveModel MaintainParallels 1 music
        fullFlatNaive = createNaiveModel FlattenParallels 1 music
        complex = createComplexModel (3,1) music

    complexStats complex

    putStrLn "Generating with Maintained Parallels"
    seqFlatGen <- naiveModelGen seqFlatNaive 50 Nothing
    putStrLn "Generating with Flattened Parallels"
    fullFlatGen <- naiveModelGen fullFlatNaive 50 Nothing
    putStrLn "Generating with Complex Model"
    complexGen <- complexModelGen complex 200

    exportMusic "seqFlat.mid" seqFlatGen
    exportMusic "fullFlat.mid" fullFlatGen
    exportMusic "complex.mid" complexGen
