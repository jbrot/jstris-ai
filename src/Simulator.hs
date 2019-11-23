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

-- garbageHistogram = M.fromList [(1,84),(2,24),(3,6),(4,18),(5,3),(7,1),(10,3)]
-- garbageTime = 3918

updateAttack :: Int -> SimulatorState -> SimulatorState
updateAttack cleared s = s{combo = cbo, attacks = atk}
    where cbo = if cleared > 0 then 1 + combo s else 0
          cboLines = case cbo - 1 of
                       -1 -> 0
                       0  -> 0
                       1  -> 0
                       2  -> 1
                       3  -> 1
                       4  -> 1
                       5  -> 2
                       6  -> 2
                       7  -> 3
                       8  -> 3
                       9  -> 4
                       10 -> 4
                       11 -> 4
                       otherwise -> 5
          clearedLines = case cleared of
                           0 -> 0
                           1 -> 0
                           2 -> 1
                           3 -> 2
                           4 -> 4
          perfectLines = if (null . V.filter (/= Empty) . board . gs $ s)
                            then 10
                            else 0
          atk = cboLines + clearedLines + perfectLines + attacks s

emptyBoard :: Board
emptyBoard = V.replicate 200 Empty

pieceQueue :: RandomGen g => g -> [Block]
pieceQueue = runIdentity . evalRandT (fmap mconcat . sequence . repeat . shuffleM $ [ I, J, L, O, S, T, Z ])

startingState :: RandomGen g => g -> SimulatorState
startingState g = SimulatorState (GameState emptyBoard (startingPosition active) Nothing queue 0) leftOver 0 0
    where (active:queue, leftOver) = splitAt 6 . pieceQueue $ g

cycleActive :: SimulatorState -> Maybe SimulatorState
cycleActive state = if canAddActiveBlock (board newGS) nact
                       then Just state{gs = newGS, squeue = buffer }
                       else Nothing
    where (n:q) = queue (gs state)
          (nq:buffer) = squeue state
          nact = startingPosition n
          newGS = (gs state){ active = startingPosition n, queue = q <> [nq] }

toggleHold :: SimulatorState -> Maybe SimulatorState
toggleHold state = case held (gs state) of
                     Nothing -> cycleActive . setHeld hld $ state
                     Just k  -> Just . setActive (startingPosition k) . setHeld hld $ state
  where hld = Just . kind . active . gs $ state 

advance :: Action -> SimulatorState -> Maybe SimulatorState
advance Hold s = toggleHold s
advance HardDrop s = cycleActive . setBoard (dropBlock (board . gs $ s) (active . gs $ s)) $ s
advance act s = Just $ setActive (moveBlock (board . gs $ s)  act (active . gs $ s)) s

simulateAI :: (MonadIO m, RandomGen g) => g -> Int -> AIState -> m (Int, Int)
simulateAI gen ct = evalStateT $ go ct st0
    where st0 = startingState gen
          disp :: MonadIO m => SimulatorState -> m ()
          disp state = liftIO . (>> putStrLn "") . printBoard . addActiveBlock (board . gs $ state) . active . gs $ state
          go :: MonadIO m => Int -> SimulatorState -> StateT AIState m (Int, Int)
          go 0 st = disp st >> pure (0, attacks st) 
          go n st' = do
              let (cleared, brd) = clearLines . board . gs $ st'
                  st = updateAttack cleared . setBoard brd $ st'
              disp st
              liftIO . putStrLn $ "Combo: " ++ (show (combo st)) ++ " Total Attack: " ++ (show (attacks st)) ++ "\n"
              actions <- runAI . gs $ st
              case foldl (\st act -> advance act =<< st) (Just st) actions of
                Just s -> go (n - 1) s
                Nothing -> pure (n, attacks st)
