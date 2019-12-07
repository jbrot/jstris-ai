module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import System.Random

import AI
import CLI
import Online
import Simulator
import Tetris.Board
import Tetris.State
import Train

main :: IO ()
main = do
    cmd <- processCLI
    case cmd of
      Run a u -> parseAISpec a >>= \a' -> runOnline a' u
      Simulate a v -> parseAISpec a >>= \a' -> runSimulation a' v
      Train a v -> runTraining a v

-----------------------
-----------------------
-- | Simulate Code | --
-----------------------
-----------------------

runSimulation :: AIState -> Bool -> IO ()
runSimulation ai v = flip evalStateT ai . go 0 . startingState =<< getStdGen
    where go :: Int -> SimulatorState -> StateT AIState IO ()
          go n st = do
              when v . liftIO . (>> putStrLn "") . printBoard . addActiveBlock (board . gs $ st) . active . gs $ st
              acts <- runAI 10 (gs st)
              let acts' :: [Maybe SimulatorState -> StateT AIState IO (Maybe SimulatorState)]
                  acts' = fmap (\(a,_) -> fmap (fmap fst . join) . sequence . fmap (advance n a)) acts
              st' <- foldl (>=>) (pure . id) acts' (Just st)
              case st' of
                Just s -> go (n + 1) s
                Nothing -> pure ()
