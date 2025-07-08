module Generation.Random
    (
        randomGen
    ) where

import Generation.Shared
import Music
import Data.Functor
import System.Random (randomRIO)

randomGen :: Probabilities -> Int -> IO Music
randomGen ps 0 = randomNote (noteProbs ps) <&> Single
randomGen ps n = do
    m <- randomMusic (musicProbs ps) 
    case m of
        (Single _) -> do
            a <- randomNote (noteProbs ps)
            return $ Single a
        (Sequential _ _) -> do
            a <- randomGen ps (n-1)
            b <- randomGen ps (n-1)
            return $ Sequential a b
        (Parallel _ _) -> do
            a <- randomGen ps (n-1)
            b <- randomGen ps (n-1)
            return $ Parallel a b
        (Repeat _ _) -> do
            a <- randomRIO (1,6)
            b <- randomGen ps (n-1)
            return $ Repeat a b
