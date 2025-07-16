module Main where

import Music
import Generation
import Codec.Midi
import System.Environment (getArgs)
import System.Directory (listDirectory)
import Data.List (intercalate, foldl')
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

mostConnectedLabel :: Graph a b -> Int
mostConnectedLabel = 
    fst . foldl findMax (0,0) . map (\(k,v) -> (k, M.size v)) . M.toList . adjList
    where
        findMax m@(_,maxCount) n@(_,count)
            | count > maxCount = n
            | otherwise = m

exportMusic :: String -> Music -> IO ()
exportMusic fname m = exportFile fname $ musicToMidi 480 m

endsWith :: String -> String -> Bool
endsWith str suffix = reverse (take (length suffix) $ reverse str) == suffix

chainFold :: Int -> IO (Graph Music Rational) -> String -> IO (Graph Music Rational)
chainFold n accIO str = do
    putStrLn str
    acc <- accIO
    music <- importMusic str
    return $ combMarkov acc (createMusicMarkov n music)

createChain :: String -> Int -> IO (Graph Music Rational)
createChain p n = do
    files <- listDirectory p

    let midiOnly = filter (`endsWith` ".mid") (take 200 files)
        fullPaths = map (\s -> p ++ "/" ++ s) midiOnly

    putStrLn "Folding"
    foldl' (chainFold n) (return emptyGraph) fullPaths

main :: IO ()
main = do
    (folderpath:nStr:lStr:_) <- getArgs

    let n = read nStr :: Int
        l = read lStr :: Int
    
    chain <- createChain folderpath n
    graphStats chain
    
    generated <- markovGen chain l Nothing
    exportMusic "fullFolder.mid" generated
