import Control.Monad.IO.Class
import Control.Monad.Random
import Criterion.Main
import System.Random

import TestMC
import Tetris
import Simulator

main = defaultMain [
       bgroup "strip" [ bench "10 step sim"   $ nfIO ((\g -> fmap fst $ runSimulation g 10) =<< getStdGen)
                      , bench "20 step sim"   $ nfIO ((\g -> fmap fst $ runSimulation g 20) =<< getStdGen)
                      ]
       ]

runSimulation :: RandomGen g => MonadIO m => g -> Int -> m (Int, g)
runSimulation g max = flip runRandT g . go 0 . startingState $ g
    where go :: (MonadRandom m, MonadIO m) => Int -> SimulatorState -> m Int
          go i s | i >= max  = pure i
                 | otherwise = do
                     (i', ms) <- stepSimulation i s
                     case ms of
                       Nothing -> pure i'
                       Just s' -> go i' s'


stepSimulation :: (MonadRandom m, MonadIO m) => Int -> SimulatorState -> m (Int, Maybe SimulatorState)
stepSimulation i s = monteCarloStep (gs s) >>= go i s
    where go :: (MonadRandom m, MonadIO m) => Int ->  SimulatorState -> [Action] -> m (Int, Maybe SimulatorState)
          go i s [] = pure (i, Just s)
          go i s (a:as) = advance i a s >>= \x -> case x of
                                                    Nothing -> pure (i, Nothing)
                                                    Just (s', i') -> go i' s' as
