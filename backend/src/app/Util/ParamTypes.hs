{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Util.ParamTypes
    (
        ComplexParameters(..),
        NaiveParameters(..)
    ) where

import GHC.Generics
import Data.Aeson
import Data.Char (toLower, isUpper)

data ComplexParameters = ComplexParameters
    {
        durationDegree :: Int,
        pitchDegree :: Int,
        genSteps :: Int,
        genCount :: Int,
        ticksPerQuarter :: Int
    }
    deriving (Generic, Show)

data NaiveParameters = NaiveParameters
    {
        degree :: Int,
        genSteps :: Int,
        genCount :: Int,
        ticksPerQuarter :: Int
    }
    deriving (Generic, Show)

instance FromJSON ComplexParameters where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelToKebab }

instance FromJSON NaiveParameters where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelToKebab }

-- Turn camelCase into kebab-case
camelToKebab :: String -> String
camelToKebab [] = []
camelToKebab (x:xs) = toLower x : go xs
    where
        go [] = []
        go (c:cs)
            | isUpper c = '-' : toLower c : go cs
            | otherwise = c : go cs
