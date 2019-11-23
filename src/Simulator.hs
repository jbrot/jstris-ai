module Simulator where

import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.State
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Random.Shuffle

import AI
import Tetris

data SimulatorState = SimulatorState { gs :: GameState
                                     , squeue :: [Block]
                                     , combo :: Int
                                     , attacks :: Int
                                     }

-- This could be done with lenses, but I don't feel like figuring out how they work right now
setBoard :: Board -> SimulatorState -> SimulatorState
setBoard b st@SimulatorState{gs = g} = st{gs = g{board = b}}
setActive :: ActiveBlock -> SimulatorState -> SimulatorState
setActive a st@SimulatorState{gs = g} = st{gs = g{active = a}}
setHeld :: Maybe Block -> SimulatorState -> SimulatorState
setHeld b st@SimulatorState{gs = g} = st{gs = g{held = b}}
setQueue :: [Block] -> SimulatorState -> SimulatorState
setQueue q st@SimulatorState{gs = g} = st{gs = g{queue = q}}

emptyBoard :: Board
emptyBoard = V.replicate 200 Empty

pieceQueue :: RandomGen g => g -> [Block]
pieceQueue = runIdentity . evalRandT (fmap mconcat . sequence . repeat . shuffleM $ [ I, J, L, O, S, T, Z ])

startingState :: RandomGen g => g -> SimulatorState
startingState g = SimulatorState (GameState emptyBoard (startingPosition active) Nothing queue) leftOver 0 0
    where (active:queue, leftOver) = splitAt 6 . pieceQueue $ g

cycleActive :: SimulatorState -> SimulatorState
cycleActive state = state{gs = newGS, squeue = buffer }
    where (n:q) = queue (gs state)
          (nq:buffer) = squeue state
          newGS = (gs state){ active = startingPosition n, queue = q <> [nq] }

toggleHold :: SimulatorState -> SimulatorState
toggleHold state = case held (gs state) of
                     Nothing -> cycleActive . setHeld hld $ state
                     Just k  -> setActive (startingPosition k) . setHeld hld $ state
  where hld = Just . kind . active . gs $ state 

advance :: SimulatorState -> Action -> SimulatorState
advance s Hold = toggleHold s
advance s HardDrop = cycleActive . setBoard (dropBlock (board . gs $ s) (active . gs $ s)) $ s
advance s act = setActive (moveBlock (board . gs $ s)  act (active . gs $ s)) s

simulateAI :: (MonadIO m, RandomGen g) => g -> Int -> AIState -> m ()
simulateAI gen ct = evalStateT $ go ct st0
    where st0 = startingState gen
          disp :: MonadIO m => SimulatorState -> m ()
          disp state = liftIO . (>> putStrLn "") . printBoard . addActiveBlock (board . gs $ state) . active . gs $ state
          go :: MonadIO m => Int -> SimulatorState -> StateT AIState m ()
          go 0 st = disp st 
          go n st' = do
              let (cleared, brd) = clearLines . board . gs $ st'
                  st = setBoard brd st'
              disp st
              actions <- runAI . gs $ st
              go (n - 1) $ foldl advance st actions
