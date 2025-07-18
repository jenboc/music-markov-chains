{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Generation.Markov (NaiveModelType(..))

import Util.FileManagement
import Util.Models

import Web.Scotty
import Network.HTTP.Types.Status (internalServerError500)
import Network.Wai.Parse (lbsBackEnd, parseRequestBody)
import Data.Aeson as A
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL

sendZip :: BL.ByteString -> ActionM ()
sendZip z = do
    setHeader "Content-Type" "application/zip"
    setHeader "Content-Disposition" "attachment; filename=\"generated.zip\""
    raw z

modelEndpoint :: FromJSON a => (a -> [FilePath] -> IO (Maybe BL.ByteString)) -> ActionM ()
modelEndpoint gen = do
    req <- request
    (fieldsPart, filesPart) <- liftIO $ parseRequestBody lbsBackEnd req

    -- Start by parsing the JSON
    let maybeJson = lookup "params" fieldsPart
    case maybeJson of
        Nothing -> text "Missing JSON part 'params'"
        Just bs -> case eitherDecode (BL.fromStrict bs) of
            Left err -> text $ "Invalid JSON: " <> TL.pack err
            Right val -> do
                filePaths <- liftIO $ saveFiles filesPart
                zipped <- liftIO $ gen val filePaths
                case zipped of
                    Nothing -> do
                        status internalServerError500
                        text "Could not open the provided files"
                    Just z -> sendZip z
main :: IO ()
main = scotty 3000 $ do
    post "/generate/complexModel" $ modelEndpoint useComplex
    post "/generate/naiveFlattenedModel" $ modelEndpoint (useNaive FlattenParallels)
    post "/generate/naiveMaintainedModel" $ modelEndpoint (useNaive MaintainParallels)
