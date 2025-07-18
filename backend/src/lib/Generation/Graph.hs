{-
    The main graph structure used to represent Markov Chains.
-}

module Generation.Graph
    (
        Graph(..),
        MarkovChain,
        Label,
        emptyGraph,
        lookupLabel,
        lookupData,
        lookupOrInsert,
        insertEdge,
        insertEdgeWith,
        unionWith,
        combMarkov,
        markovFromFreqGraph,
        createTransFreqGraph,
        markovGen
    ) where

import Music
import Generation.Shared
import System.Random (randomRIO)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (foldl')
import Data.Ratio (numerator, denominator, (%))
import Data.Bifunctor (second)
import qualified Data.Map as M
import qualified Data.Map.Strict as MS

-- Type Definitions
type Label = Int
type AdjacencyList a = M.Map Label (M.Map Label a)
type MarkovChain a = Graph a Rational

data Graph a b = Graph
    {
        labelToDataMap :: M.Map Label a,
        dataToLabelMap :: M.Map a Label,
        adjList :: AdjacencyList b,
        nextLabel :: Label
    }
    deriving Eq

instance Functor (Graph a) where
    fmap f g@(Graph { adjList = l }) =
        g { adjList = MS.map (fmap f) l }

emptyGraph :: Graph a b
emptyGraph = Graph
    {
        labelToDataMap = M.empty,
        dataToLabelMap = M.empty,
        adjList = M.empty,
        nextLabel = 0
    }

-- Graph Operations
-- What is the label for this piece of data?
lookupLabel :: Ord a => a -> Graph a b -> Maybe Label
lookupLabel a g = MS.lookup a (dataToLabelMap g)

-- What is the data associated with this label?
lookupData :: Ord a => Label -> Graph a b -> Maybe a
lookupData l g = MS.lookup l (labelToDataMap g)

-- Add a piece of data to the graph
insertData :: Ord a => a -> Graph a b -> Graph a b
insertData a g = let l = nextLabel g in case lookupLabel a g of
    Just l -> g
    Nothing -> 
        Graph
            {
                labelToDataMap = MS.insert l a (labelToDataMap g),
                dataToLabelMap = MS.insert a l (dataToLabelMap g),
                adjList = MS.insert l M.empty (adjList g),
                nextLabel = l + 1
            }

-- Only insert a piece of data if it isn't already in the graph
lookupOrInsert :: Ord a => a -> Graph a b -> (Graph a b, Label)
lookupOrInsert a g = let g' = insertData a g in case lookupLabel a g' of
    Nothing -> error "Insertion failed"
    Just l -> (g',l)

-- Insert an edge between two pieces of data
-- If any of the nodes aren't in the graph, add them
-- If the edge already exists, overwrite it
insertEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
insertEdge from to weight g =
    let (g', fromLabel) = lookupOrInsert from g
        (g'', toLabel) = lookupOrInsert to g'
        adjList' = MS.alter (addEdge toLabel weight) fromLabel (adjList g'')
    in g'' { adjList = adjList' } 
    where
        addEdge l w Nothing = Just $ M.singleton l w
        addEdge l w (Just innerMap) = Just $ M.insert l w innerMap

-- Insert an edge between two pieces of data
-- If any of the nodes aren't in the graph, add them
-- If the edge already exists, combine the old and new weight with a given function
insertEdgeWith :: Ord a => (b -> b -> b) -> a -> a -> b -> Graph a b -> Graph a b
insertEdgeWith f from to weight g =
    let (g', fromLabel) = lookupOrInsert from g
        (g'', toLabel) = lookupOrInsert to g'
        adjList' = MS.alter (addEdge toLabel weight) fromLabel (adjList g'')
    in g'' { adjList = adjList' }
    where
        addEdge l w Nothing = Just $ M.singleton l w
        addEdge l w (Just innerMap) = Just $ M.insertWith f l w innerMap

-- Combine two graphs together into one structure
-- If an edge exists in both graphs, then combine their weights with the given function
unionWith :: (Ord a, Eq a) => (b -> b -> b) -> Graph a b -> Graph a b -> Graph a b
unionWith f a b = 
    let aLabels = MS.keys (labelToDataMap a)
        (g, aLabelToBLabel) = foldl' addLabels (b,M.empty) aLabels
        aAdjList = replaceKeys (adjList a) aLabelToBLabel
    in g {adjList = MS.unionWith (MS.unionWith f) aAdjList (adjList g)}
    where
        addLabels (gAcc, mAcc) aLabel = 
            let aData = case lookupData aLabel a of
                    Nothing -> error "The label doesn't have any data attached to it!"
                    Just d -> d
                (gAcc',bLabel) = lookupOrInsert aData gAcc
                mAcc' = MS.insert aLabel bLabel mAcc
            in (gAcc', mAcc')

        replaceKeys m labelMap = MS.fromList 
            [
                (newKey,v) 
                | (oldKey,v) <- MS.toList m, 
                  Just newKey <- [MS.lookup oldKey labelMap]
            ]

-- Combine two markov chains by combining the probabilities
combMarkov :: (Ord a, Eq a) => MarkovChain a -> MarkovChain a -> MarkovChain a
combMarkov = unionWith comb
    where
        comb x y = (numerator x + numerator y) % (denominator x + denominator y)

-- Given a frequency graph, construct a Markov Chain
markovFromFreqGraph :: Ord a => Graph a Integer -> MarkovChain a
markovFromFreqGraph g@(Graph {adjList = l}) = g {adjList = MS.map normalise l}
    where
        normalise innerMap = 
            let total = MS.foldl' (+) 0 innerMap
            in MS.map (% total) innerMap

-- Given a sequence of data, construct a frequency graph of the different transitions
-- between the data
createTransFreqGraph :: Ord a => [a] -> Graph a Integer
createTransFreqGraph = build emptyGraph
    where
        build acc [] = acc
        build acc [x] = insertData x acc
        build acc (from:to:r) = insertEdgeWith (+) from to 1 (build acc (to:r))

-- Walk through the Markov Chain to generate some data
markovGen :: MarkovChain a -> Int -> Maybe Label -> IO [a]
markovGen g n (Just start) = case M.lookup start (labelToDataMap g) of
    Nothing -> markovGen g n Nothing
    _ -> do
        generatedLabels <- go n start
        let generatedData = mapMaybe (`M.lookup` labelToDataMap g) generatedLabels
        return generatedData
    where
        go :: Int -> Label -> IO [Label]
        go 0 _ = return []
        go n curr = do
            let adj = M.toList $ M.findWithDefault M.empty curr (adjList g)

            if null adj
                then return [curr]
                else do
                    nextLabel <- chooseFromProbList $ map (second fromRational) adj
                    listTail <- go (n-1) nextLabel
                    return (curr:listTail)
markovGen g n Nothing = chooseStart >>= markovGen g n . Just
    where
        chooseStart :: IO Label
        chooseStart = do
            let labels = M.keys (labelToDataMap g)
            n <- randomRIO (0, length labels)
            return $ labels !! n
