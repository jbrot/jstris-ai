import Control.Monad.IO.Class
import Control.Monad.Random
import Criterion.Main

import TestMC
import Tetris.Action
import Simulator

main = defaultMain [
       bgroup "strip" [ bench "5 step sim"   $ nfIO ((\g -> fmap fst $ runSimulation g 5) =<< getStdGen)
                      , bench "10 step sim"   $ nfIO ((\g -> fmap fst $ runSimulation g 10) =<< getStdGen)
                      , bench "20 step sim"   $ nfIO ((\g -> fmap fst $ runSimulation g 20) =<< getStdGen)
                      ]
       ]

runSimulation :: RandomGen g => MonadIO m => g -> Int -> m (Int, g)
runSimulation g max = flip runRandT g . go 0 . startingState $ g
    where go :: (MonadRandom m, MonadIO m) => Int -> SimulatorState -> m Int
          go i s | i >= max  = pure i
                 | otherwise = do
                     ms <- stepSimulation i s
                     case ms of
                       Nothing -> pure i
                       Just s' -> go (i + 1) s'


stepSimulation :: (MonadRandom m, MonadIO m) => Int -> SimulatorState -> m (Maybe SimulatorState)
stepSimulation i s = monteCarloStep (gs s) >>= go s
    where go :: (MonadRandom m, MonadIO m) => SimulatorState -> [Action] -> m (Maybe SimulatorState)
          go s [] = pure (Just s)
          go s (a:as) = advance i a s >>= \x -> case x of
                                                    Nothing -> pure Nothing
                                                    Just (s', _) -> go s' as
