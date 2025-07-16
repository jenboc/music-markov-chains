module Generation.Bayes
    (
        Graph(..),
        MarkovChain,
        emptyGraph,
        createMusicMarkov,
        markovGen,
        combMarkov
    ) where

import Music
import Generation.Shared
import System.Random (randomRIO)
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Data.Ratio (numerator, denominator, (%))
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
lookupLabel :: Ord a => a -> Graph a b -> Maybe Label
lookupLabel a g = MS.lookup a (dataToLabelMap g)

lookupData :: Ord a => Label -> Graph a b -> Maybe a
lookupData l g = MS.lookup l (labelToDataMap g)

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

lookupOrInsert :: Ord a => a -> Graph a b -> (Graph a b, Label)
lookupOrInsert a g = let g' = insertData a g in case lookupLabel a g' of
    Nothing -> error "Insertion failed"
    Just l -> (g',l)

insertEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
insertEdge from to weight g =
    let (g', fromLabel) = lookupOrInsert from g
        (g'', toLabel) = lookupOrInsert to g'
        adjList' = MS.alter (addEdge toLabel weight) fromLabel (adjList g'')
    in g'' { adjList = adjList' } 
    where
        addEdge l w Nothing = Just $ M.singleton l w
        addEdge l w (Just innerMap) = Just $ M.insert l w innerMap

insertEdgeWith :: Ord a => (b -> b -> b) -> a -> a -> b -> Graph a b -> Graph a b
insertEdgeWith f from to weight g =
    let (g', fromLabel) = lookupOrInsert from g
        (g'', toLabel) = lookupOrInsert to g'
        adjList' = MS.alter (addEdge toLabel weight) fromLabel (adjList g'')
    in g'' { adjList = adjList' }
    where
        addEdge l w Nothing = Just $ M.singleton l w
        addEdge l w (Just innerMap) = Just $ M.insertWith f l w innerMap

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


-- Constructing a Markov Chain
markovFromFreqGraph :: Ord a => Graph a Integer -> MarkovChain a
markovFromFreqGraph g@(Graph {adjList = l}) = g {adjList = MS.map normalise l}
    where
        normalise innerMap = 
            let total = MS.foldl' (+) 0 innerMap
            in MS.map (% total) innerMap

createTransFreqGraph :: Ord a => [a] -> Graph a Integer
createTransFreqGraph = build emptyGraph
    where
        build acc [] = acc
        build acc [x] = insertData x acc
        build acc (from:to:r) = insertEdgeWith (+) from to 1 (build acc (to:r))

createMusicMarkov :: Int -> Music -> Graph Music Rational
createMusicMarkov n = markovFromFreqGraph . createTransFreqGraph . collect n . flatten
    where
        flatten :: Music -> [Music]
        flatten (Parallel a b) = flatten a ++ flatten b
        flatten (Sequential a b) = flatten a ++ flatten b
        flatten r@(Repeat _ _) = flatten $ expandRepeat r
        flatten m = [m]

        collect _ [] = []
        collect n xs = fromMaybe (Single $ Rest Whole) (sequentialise $ take n xs) : collect n (drop n xs)

-- Combining a Markov Chain
combMarkov :: (Ord a, Eq a) => MarkovChain a -> Graph a Rational -> Graph a Rational
combMarkov = unionWith comb
    where
        comb x y = (numerator x + numerator y) % (denominator x + denominator y)

-- Generating a Markov Chain
markovGen :: Graph Music Rational -> Int -> Maybe Label -> IO Music
markovGen g n (Just start) = case M.lookup start (labelToDataMap g) of
    Nothing -> markovGen g n Nothing
    _ -> do
        labels <- generate n start
        let blocks = map (\l -> fromMaybe (Single $ Rest Whole) $ M.lookup l (labelToDataMap g)) labels
            sequenced = fromMaybe (Single $ Rest Whole) $ sequentialise blocks
        return sequenced
    where
        generate :: Int -> Label -> IO [Label]
        generate 0 _ = return []
        generate n curr = do
            let adj = M.toList $ M.findWithDefault M.empty curr (adjList g)

            if null adj 
                then return [curr]
                else do
                    nextLabel <- chooseFromProbList $ map (\(l,p) -> (l, fromRational p)) adj
                    listTail <- generate (n-1) nextLabel
                    return (curr:listTail)
markovGen g n Nothing = chooseStart >>= markovGen g n . Just    
    where
        chooseStart :: IO Label
        chooseStart = do
            let labels = M.keys (labelToDataMap g)
            n <- randomRIO (0, length labels)
            return $ labels !! n


