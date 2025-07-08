module Generation.Bayes
    (
        BayesNet(..),
        bayesGen
    ) where

import Generation.Shared
import Music
import Data.Maybe (fromMaybe)

-- ADT for a Bayes Network
-- A node has data and probabilistic connections
data BayesNet a = Node a [(BayesNet a, Double)]

-- TODO: Generate a Bayes model of a given music structure

-- Walk through a Bayes Network and generate music
bayesGen :: BayesNet Music -> Int -> IO Music
bayesGen net n = fromMaybe (Single $ Rest Whole) . sequentialise 
    <$> bayesGen' [] net n
    where
        bayesGen' :: [Music] -> BayesNet Music -> Int -> IO [Music]
        bayesGen' l _ 0 = return $ reverse l
        bayesGen' l (Node _ conns) n = do
            newNet@(Node m _) <- chooseFromProbList conns
            bayesGen' (m:l) newNet (n-1)
