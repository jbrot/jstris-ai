module Simulator (SimulatorState(..), startingState, advance) where

import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.Trans.State.Strict
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import System.Random.Shuffle

import Tetris
import Tetris.Block
import Tetris.Board

data SimulatorState = SimulatorState { gs :: GameState
                                     , squeue :: [Block]
                                     , combo :: Int
                                     }

updateGS :: (GameState -> GameState) -> SimulatorState -> SimulatorState
updateGS f s = s{gs = f . gs $ s}

-- This could be done with lenses, but I don't feel like figuring out how they work right now
setBoard :: Board -> SimulatorState -> SimulatorState
setBoard b = updateGS $ \g -> g{board = b}
setActive :: ActiveBlock -> SimulatorState -> SimulatorState
setActive a = updateGS $ \g -> g{active = a}
setHeld :: Maybe Block -> SimulatorState -> SimulatorState
setHeld b = updateGS $ \g -> g{held = b}
setQueue :: [Block] -> SimulatorState -> SimulatorState
setQueue q = updateGS $ \g -> g{queue = q}

garbageHistogram = [(1,517),(2,111),(3,27),(4,52),(5,16),(7,1),(10,3)]
garbageTime = 10081

sampleHistogram :: MonadRandom m => [(a, Int)] -> m a 
sampleHistogram h = fmap (\v -> fromJust . snd . foldl (flip iterate) (v, Nothing) $ h) $ getRandomR (0, len - 1)
    where len = sum . fmap snd $ h
          iterate :: (a, Int) -> (Int, Maybe a) -> (Int, Maybe a)
          iterate _ (_, Just a) = (0, Just a)
          iterate (a, c) (r, Nothing) = if c >= r then (0, Just a)
                                                  else (r - c, Nothing)

queueGarbage :: MonadRandom m => SimulatorState -> m SimulatorState
queueGarbage s = getRandomR (0, garbageTime) >>= \r -> 
    if r > (sum . fmap snd $ garbageHistogram)
       then pure s
       else do
           ct <- sampleHistogram garbageHistogram
           queueGarbage . updateGS (\g -> g{garbage = garbage g <> [ct]}) $ s

updateAttack :: Int -> SimulatorState -> (SimulatorState, Int)
updateAttack cleared s = (s{combo = cbo}, attackLines (board . gs $ s) cbo cleared)
    where cbo = if cleared > 0 then 1 + combo s else 0

pieceQueue :: RandomGen g => g -> [Block]
pieceQueue = runIdentity . evalRandT (fmap mconcat . sequence . repeat . shuffleM $ [ I, J, L, O, S, T, Z ])

startingState :: RandomGen g => g -> SimulatorState
startingState g = SimulatorState (GameState emptyBoard (startingPosition active) Nothing queue []) leftOver 0
    where (active:queue, leftOver) = splitAt 6 . pieceQueue $ g

cycleActive :: SimulatorState -> Maybe SimulatorState
cycleActive state = if canAddActiveBlock (board newGS) nact
                       then Just state{gs = newGS, squeue = buffer }
                       else Nothing
    where (n:q) = queue (gs state)
          (nq:buffer) = squeue state
          nact = startingPosition n
          newGS = (gs state){ active = startingPosition n, queue = q <> [nq] }

applyGarbage :: MonadRandom m => Int -> SimulatorState -> m SimulatorState
applyGarbage 0 = \s -> do g <- addGarbage . gs $ s
                          pure . updateGS (const g) $ s
applyGarbage n = pure . updateGS (reduceGarbage n)

applyHurryUp :: Int -> SimulatorState -> SimulatorState
applyHurryUp n s
  | n < 900 = s
  | n `mod` 20 == 0 = setBoard (hurryUp 1 . board . gs $ s) s
  | otherwise = s

advanceBoard :: MonadRandom m => Int -> SimulatorState -> m (SimulatorState, Int)
advanceBoard n state = fmap (updateAttack cleared) . join . fmap (queueGarbage . applyHurryUp n) . applyGarbage cleared . setBoard brd $ state
    where (cleared, brd) = clearLines . board . gs $ state

toggleHold :: SimulatorState -> Maybe SimulatorState
toggleHold state = case held (gs state) of
                     Nothing -> cycleActive . setHeld hld $ state
                     Just k  -> Just . setActive (startingPosition k) . setHeld hld $ state
  where hld = Just . kind . active . gs $ state 

advance :: MonadRandom m => Int -> Action -> SimulatorState -> m (Maybe (SimulatorState, Int))
advance _ Hold s = pure . fmap (flip (,) 0) . toggleHold $ s
advance n HardDrop s = fmap (fmap swap . sequence . fmap cycleActive . swap) . advanceBoard n . setBoard (dropBlock (board . gs $ s) (active . gs $ s)) $ s
advance _ act s = pure . Just . flip (,) 0 . setActive (moveBlock' (board . gs $ s)  act (active . gs $ s)) $ s
