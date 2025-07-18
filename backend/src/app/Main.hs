{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Music
import Generation

import GHC.Generics
import Web.Scotty hiding (File)
import Network.HTTP.Types.Status (internalServerError500)
import Network.Wai.Parse (File, FileInfo(..), lbsBackEnd, parseRequestBody)
import Codec.Archive.Zip
import Control.Monad (forM, foldM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Data.Char (toLower, isUpper)
import Data.Aeson as A
import System.Directory (createDirectoryIfMissing, removeFile, doesFileExist)
import System.FilePath ((</>), takeFileName)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as MS

data ComplexParameters = ComplexParameters
    {
        durationDegree :: Int,
        pitchDegree :: Int,
        genSteps :: Int,
        genCount :: Int
    }
    deriving (Generic, Show)

instance FromJSON ComplexParameters where
    parseJSON = genericParseJSON A.defaultOptions
        { fieldLabelModifier = camelToKebab }

camelToKebab :: String -> String
camelToKebab [] = []
camelToKebab (x:xs) = toLower x : go xs
    where
        go [] = []
        go (c:cs)
            | isUpper c = '-' : toLower c : go cs
            | otherwise = c : go cs

tmpUploadsDir :: String
tmpUploadsDir = "uploads"

tmpGeneratedDir :: String
tmpGeneratedDir = "generated"

saveFile :: FilePath -> (BS.ByteString, FileInfo BL.ByteString) -> IO FilePath
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

saveFiles :: [File BL.ByteString] -> IO [FilePath]
saveFiles files = do
    createDirectoryIfMissing True tmpUploadsDir
    mapM (saveFile tmpUploadsDir) files

sendZip :: BL.ByteString -> ActionM ()
sendZip z = do
    setHeader "Content-Type" "application/zip"
    setHeader "Content-Disposition" "attachment; filename=\"generated.zip\""
    raw z

useComplex :: ComplexParameters -> [FilePath] -> IO (Maybe BL.ByteString)
useComplex (ComplexParameters durDeg pitchDeg steps count) fpaths = do
    model <- foldl' (foldComplexChain (durDeg, pitchDeg)) (return emptyComplexModel) fpaths

    -- If after all that we still have the empty model, we failed to open
    -- every file
    if model == emptyComplexModel
        then return Nothing
        else do
            createDirectoryIfMissing True tmpGeneratedDir
            -- Generate and save the files
            generatedPaths <- forM [1..count] $ \i -> do
                music <- complexModelGen model steps
                let outPath = tmpGeneratedDir </> ("output_" ++ show i ++ ".mid")
                exportMusicToMidi outPath music tpq
                return outPath

            z <- zipFiles generatedPaths

            -- Delete the training and generated files
            mapM_ (\f -> doesFileExist f >>= \e -> when e (removeFile f)) (fpaths ++ generatedPaths)

            return $ Just z

main :: IO ()
main = scotty 3000 $ do
    post "/generate/complexModel" $ do
        liftIO $ putStrLn "REQUEST RECEIVED"

        req <- request
        (fieldsPart, filesPart) <- liftIO $ parseRequestBody lbsBackEnd req

        -- Start by parsing the JSON
        let maybeJson = lookup "params" fieldsPart
        case maybeJson of
            Nothing -> text "Missing JSON part 'params'"
            Just bs -> case eitherDecode (BL.fromStrict bs) of
                Left err -> text $ "Invalid JSON: " <> TL.pack err
                Right val -> do
                    let params = (val :: ComplexParameters)
                    filePaths <- liftIO $ saveFiles filesPart
                    zipped <- liftIO $ useComplex params filePaths
                    case zipped of
                        Nothing -> do
                            status internalServerError500
                            text "Could not open the provided files"
                        Just z -> sendZip z
