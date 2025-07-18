module Generation.Markov
    (
        NaiveModelType(..),
        NaiveModel,
        ComplexModel(..),
        emptyNaiveModel,
        emptyComplexModel,
        createNaiveModel,
        naiveModelGen,
        combNaiveModel,
        createComplexModel,
        combComplexModel,
        complexModelGen
    ) where

import Music
import Generation.Shared
import Generation.Graph
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad ((>=>))

-- Models using Markov Chain
data NaiveModelType = FlattenParallels | MaintainParallels
    deriving (Enum, Eq)
newtype NaiveModel = NaiveModel (MarkovChain Music)
    deriving Eq

-- Complex Models flatten parallels by default
data ComplexModel = ComplexModel
    {
        -- Using Markov Chains of lists since this allows us to represent
        -- higher degree chains
        durationChain :: MarkovChain [Duration],
        pitchChain :: MarkovChain [Pitch]
        -- Could consider on/off chain -- but would need to be high degree to be useful
    }
    deriving Eq

emptyNaiveModel :: NaiveModel
emptyNaiveModel = NaiveModel emptyGraph

emptyComplexModel :: ComplexModel
emptyComplexModel = ComplexModel emptyGraph emptyGraph

-- Naive Model Construction and Generation
createNaiveModel :: NaiveModelType -> Int -> Music -> NaiveModel
createNaiveModel t n = NaiveModel . markovFromFreqGraph . createTransFreqGraph . 
    collect n . flatten . canonicalForm
    where
        flatten :: Music -> [Music]
        flatten
            | t == FlattenParallels = flattenSequentials >=> flattenParallels
            | otherwise = flattenSequentials

        collect _ [] = []
        collect n xs = fromMaybe nullMusic (sequentialise $ take n xs) : collect n (drop n xs)

combNaiveModel :: NaiveModel -> NaiveModel -> NaiveModel
combNaiveModel (NaiveModel c1) (NaiveModel c2) = NaiveModel $ combMarkov c1 c2

naiveModelGen :: NaiveModel -> Int -> Maybe Label -> IO Music
naiveModelGen (NaiveModel chain) n maybeStart = do
    music <- markovGen chain n maybeStart
    return $ fromMaybe nullMusic $ sequentialise music

-- Complex Model Construction and Generation
createComplexModel :: (Int,Int) -> Music -> ComplexModel
createComplexModel (dn,pn) m =
    let flattened = flattenSequentials (canonicalForm m) >>= flattenParallels
        durations = mapMaybe getDuration flattened
        pitches = mapMaybe getPitch flattened
        durationGroups = collect dn durations
        pitchGroups = collect pn pitches
        durationChain = markovFromFreqGraph $ createTransFreqGraph durationGroups
        pitchesChain = markovFromFreqGraph $ createTransFreqGraph pitchGroups
    in ComplexModel durationChain pitchesChain
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

        -- Collect into groups of n (groups overlap)
        -- We want the last element in the group to be the first one
        -- since this is the one we will be accessing later (so will have O(1) access)
        collect _ [] = []
        collect n xs = reverse (take n xs) : collect n (drop 1 xs)

combComplexModel :: ComplexModel -> ComplexModel -> ComplexModel
combComplexModel (ComplexModel d1 p1) (ComplexModel d2 p2) =
    let d = combMarkov d1 d2
        p = combMarkov p1 p2
    in ComplexModel d p

complexModelGen :: ComplexModel -> Int -> IO Music
complexModelGen (ComplexModel dChain pChain) n = do
    durationGroups <- markovGen dChain n Nothing
    pitchGroups <- markovGen pChain n Nothing
    
    let pitches = map head pitchGroups
        durations = map head durationGroups
        notes = zipWith Note pitches durations
        seqd = fromMaybe nullMusic $ sequentialise $ map Single notes

    return seqd
