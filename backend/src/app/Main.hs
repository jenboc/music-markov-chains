{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Music
import Generation

import Web.Scotty
import Network.HTTP.Types.Status (internalServerError500)
import Network.Wai.Parse (FileInfo(..))
import Codec.Archive.Zip
import Control.Monad (forM, foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as MS

tmpUploadsDir :: String
tmpUploadsDir = "uploads"

tmpGeneratedDir :: String
tmpGeneratedDir = "generated"

saveFile :: FilePath -> (T.Text, FileInfo BL.ByteString) -> IO FilePath
saveFile dir (_, fileInfo) = do
    let fname = T.unpack (TE.decodeUtf8 $ fileName fileInfo)
        path = dir </> fname
    BL.writeFile path (fileContent fileInfo)
    return path

foldComplexChain :: (Int,Int) -> IO ComplexModel -> FilePath -> IO ComplexModel
foldComplexChain degs accIO filePath = do
    acc <- accIO
    maybeMusic <- importMusicFromMidi filePath

    case maybeMusic of
        Nothing -> accIO
        Just m -> 
            let model = createComplexModel degs m
            in return $ combComplexModel acc model

tpq :: Int
tpq = 480

zipFiles :: [FilePath] -> IO BL.ByteString
zipFiles filePaths = do
    archive <- foldM addToArchive emptyArchive filePaths
    return $ fromArchive archive
    where
        addToArchive :: Archive -> FilePath -> IO Archive
        addToArchive a path = do
            content <- BL.readFile path
            let entryName = takeFileName path
                entry = toEntry entryName 0 content
            return $ addEntryToArchive entry a

main :: IO ()
main = scotty 3000 $ do
    post "/generate/complexModel" $ do
        durationDegree :: Int <- param "duration-degree"
        pitchDegree :: Int <- param "pitch-degree"
        genSteps :: Int <- param "gen-steps"
        genCount :: Int <- param "gen-count"

        midiFiles <- files

        -- Put uploaded files in tmp directory
        liftIO $ createDirectoryIfMissing True tmpUploadsDir
        liftIO $ createDirectoryIfMissing True tmpGeneratedDir
        savedPaths <- liftIO $ mapM (saveFile tmpUploadsDir) midiFiles

        let degs = (durationDegree, pitchDegree)
        model <- liftIO $ foldl' (foldComplexChain degs) (return emptyComplexModel) savedPaths

        -- If after all that we still have the empty model, we failed to open
        -- every file
        if model == emptyComplexModel
            then do
                status internalServerError500
                text "Could not open the provided files."
            else do
                -- Generate and save the files
                generatedPaths <- forM [1..genCount] $ \i -> do
                    music <- liftIO $ complexModelGen model genSteps
                    let outPath = tmpGeneratedDir </> ("output_" ++ show i ++ ".mid")
                    liftIO $ exportMusicToMidi outPath music tpq
                    return outPath

                zipData <- liftIO $ zipFiles generatedPaths

                setHeader "Content-Type" "application/zip"
                setHeader "Content-Disposition" "attachment; filename=\"generated.zip\""
                raw zipData
