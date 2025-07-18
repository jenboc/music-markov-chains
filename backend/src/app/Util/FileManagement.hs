module Util.FileManagement
    (
        uploadsDir,
        generatedDir,
        saveFiles,
        zipFiles
    ) where

import Codec.Archive.Zip
import Control.Monad (foldM)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)
import Network.Wai.Parse (File, FileInfo(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

uploadsDir :: String
uploadsDir = "uploads"

generatedDir :: String
generatedDir = "generated"

saveFiles :: [File BL.ByteString] -> IO [FilePath]
saveFiles files = do
    createDirectoryIfMissing True uploadsDir
    mapM (save uploadsDir) files
    where
        save :: FilePath -> (BS.ByteString, FileInfo BL.ByteString) -> IO FilePath
        save dir (_, fileInfo) = do
            let fname = T.unpack (TE.decodeUtf8 $ fileName fileInfo)
                path = dir </> fname
            BL.writeFile path (fileContent fileInfo)
            return path

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
