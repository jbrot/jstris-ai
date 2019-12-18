import Control.Monad.IO.Class
import Control.Monad.Random
import Criterion.Main

import MCTS
import Tetris.Action
import Tetris.Simulator

main = defaultMain [
          bgroup "strip" [ bench "5 step sim"   $ nfIO ((\g -> fmap fst $ runSimulation g 5) =<< getStdGen)
                         , bench "10 step sim"   $ nfIO ((\g -> fmap fst $ runSimulation g 10) =<< getStdGen)
                         , bench "20 step sim"   $ nfIO ((\g -> fmap fst $ runSimulation g 20) =<< getStdGen)
                         , bench "5 step sim a"   $ nfIO ((\g -> fmap fst $ runSimulation2 g 5) =<< getStdGen)
                         , bench "10 step sim a"   $ nfIO ((\g -> fmap fst $ runSimulation2 g 10) =<< getStdGen)
                         , bench "20 step sim a"   $ nfIO ((\g -> fmap fst $ runSimulation2 g 20) =<< getStdGen)
                         ]
       ]

runSimulation :: RandomGen g => MonadIO m => g -> Int -> m (Reward, g)
runSimulation g mx = flip runRandT g . simulateU mct mx . gs . startingState $ g
    where mct = MCTS { linesToReward = fromInteger . toInteger
                     , stateToReward = const 0
                     , simulate = const (pure 0)
                     , lossReward = 0
                     , gamma = 1
                     , cp = 1 / sqrt 2
                     }

runSimulation2 :: RandomGen g => MonadIO m => g -> Int -> m (Reward, g)
runSimulation2 g mx = flip runRandT g . simulateA mct mx . gs . startingState $ g
    where mct = MCTS { linesToReward = fromInteger . toInteger
                     , stateToReward = const 0
                     , simulate = const (pure 0)
                     , lossReward = 0
                     , gamma = 1
                     , cp = 1 / sqrt 2
                     }
