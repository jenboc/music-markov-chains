module Util.Models
    (
        useComplex,
        useNaive
    ) where

import Music
import Generation

import Util.ParamTypes
import Util.FileManagement

import Data.List (foldl')
import Control.Monad (forM, when)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as BL

useNaive :: NaiveModelType -> NaiveParameters -> [FilePath] -> IO (Maybe BL.ByteString)
useNaive t (NaiveParameters deg steps count tpq) fpaths =
    let gen m s = naiveModelGen m s Nothing
    in useModel (foldNaiveChain t deg) emptyNaiveModel gen fpaths steps count tpq

useComplex :: ComplexParameters -> [FilePath] -> IO (Maybe BL.ByteString)
useComplex (ComplexParameters durDeg pitchDeg steps count tpq) fpaths =
    useModel (foldComplexChain (durDeg, pitchDeg)) emptyComplexModel complexModelGen
        fpaths steps count tpq

useModel :: (Eq a) => (IO a -> FilePath -> IO a) -> a -> (a -> Int -> IO Music) -> [FilePath] -> Int -> Int -> Int -> IO (Maybe BL.ByteString)
useModel foldOp emptyModel genFunc fpaths steps count tpq = do
    model <- foldl' foldOp (return emptyModel) fpaths

    -- If after all that we still have the empty model, we failed to open
    -- every file
    if model == emptyModel
        then return Nothing
        else do
            createDirectoryIfMissing True generatedDir
            -- Generate and save the files
            generatedPaths <- forM [1..count] $ \i -> do
                music <- genFunc model steps
                let outPath = generatedDir </> ("output_" ++ show i ++ ".mid")
                exportMusicToMidi outPath music tpq
                return outPath

            z <- zipFiles generatedPaths

            -- Delete the training and generated files
            mapM_ (\f -> doesFileExist f >>= \e -> when e (removeFile f)) 
                (fpaths ++ generatedPaths)

            return $ Just z

foldNaiveChain :: NaiveModelType -> Int -> IO NaiveModel -> FilePath -> IO NaiveModel
foldNaiveChain t deg accIO filePath = do
    acc <- accIO
    maybeMusic <- importMusicFromMidi filePath

    case maybeMusic of
        Nothing -> accIO
        Just m ->
            let model = createNaiveModel t deg m
            in return $ combNaiveModel acc model

foldComplexChain :: (Int,Int) -> IO ComplexModel -> FilePath -> IO ComplexModel
foldComplexChain degs accIO filePath = do
    acc <- accIO
    maybeMusic <- importMusicFromMidi filePath

    case maybeMusic of
        Nothing -> accIO
        Just m -> 
            let model = createComplexModel degs m
            in return $ combComplexModel acc model
