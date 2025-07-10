module Generation.Bayes
    (
        BayesNet(..),
        bayesGen,
        findNode,
        createBayesMusicNet
    ) where

import Generation.Shared
import Music
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

-- ADT for a Bayes Network
-- A node has data and probabilistic connections
data BayesNet a = Node a [(BayesNet a, Double)] | Empty
    deriving Show
type TransFreqTable a = M.Map a (M.Map a Int)

-- Create a bayes network for a given piece of music using phrases of length
-- n
createBayesMusicNet :: Int -> Music -> BayesNet Music
createBayesMusicNet n m 
    | n <= 0 = Empty
    | otherwise = let phrases = conjoin n $ flatten $ canonicalForm m
                      freqTable = collectData phrases
                  in case phrases of
                    [] -> Empty
                    (f:_) -> construct freqTable M.empty f
    where
        -- Flatten the structure into a list of subtrees played in sequence
        flatten :: Music -> [Music]
        flatten r@(Repeat _ _) = flatten $ expandRepeat r
        flatten (Sequential a b) = flatten a ++ flatten b
        -- END GOAL IS TO NOT HAVE TO SPLIT PARALLEL (BUT IS NEEDED DUE TO HOW
        -- I READ MIDI FILES)
        flatten (Parallel a b) = flatten a ++ flatten b
        flatten m = [m]

        -- Sequentialise sublists of length n
        conjoin :: Int -> [Music] -> [Music]
        conjoin n [] = []
        conjoin n ms = let s = fromMaybe (Single $ Rest Whole) (sequentialise $ take n ms)
            in s : conjoin n (drop n ms)

        -- Collect transition data from the phrase list
        collectData :: [Music] -> TransFreqTable Music
        collectData ms = let transes = zip ms (tail ms)
            in foldr addTrans M.empty transes

        -- Add a transition to the table
        addTrans :: (Music,Music) -> TransFreqTable Music -> TransFreqTable Music
        addTrans (a,b) = M.insertWith (M.unionWith (+)) a (M.singleton b 1) 

        -- Check if we've constructed a node for m
        -- If we have, we can stop
        -- Otherwise, construct a node and check its connections recursively
        construct :: TransFreqTable Music -> M.Map Music (BayesNet Music) -> Music -> BayesNet Music
        construct table nodeMap m = case M.lookup m nodeMap of
            Just node -> node
            Nothing ->
                let succs = M.findWithDefault M.empty m table
                    total = fromIntegral $ sum $ M.elems succs
                    edges = [(construct table nodeMap' m', fromIntegral c / total)
                                | (m',c) <- M.toList succs]
                    node = Node m edges
                    nodeMap' = M.insert m node nodeMap
                in node

-- Depth First Search to find a node with given data
findNode :: Eq a => a -> BayesNet a -> Maybe (BayesNet a)
findNode _ Empty = Nothing
findNode x n@(Node y cs)
    | x == y = Just n
    | otherwise = walkChildren cs
    where
        walkChildren [] = Nothing
        walkChildren ((c,_):r) = case findNode x c of
            Just n' -> Just n'
            Nothing -> walkChildren r

-- Walk through a Bayes Network and generate music
bayesGen :: BayesNet Music -> Int -> IO Music
bayesGen net n = fromMaybe (Single $ Rest Whole) . sequentialise 
    <$> bayesGen' [] net n
    where
        bayesGen' :: [Music] -> BayesNet Music -> Int -> IO [Music]
        bayesGen' l _ 0 = return $ reverse l
        bayesGen' l (Node _ []) _ = return $ reverse l
        bayesGen' l (Node _ conns) n = do
            newNet@(Node m _) <- chooseFromProbList conns
            bayesGen' (m:l) newNet (n-1)
        bayesGen' l Empty _ = return $ reverse l
