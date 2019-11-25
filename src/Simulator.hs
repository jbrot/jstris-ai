module Simulator where

import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.Trans.State.Strict
import Data.Maybe (fromJust)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import System.Random.Shuffle

import AI
import Tetris

data SimulatorState = SimulatorState { gs :: GameState
                                     , squeue :: [Block]
                                     , combo :: Int
                                     , attacks :: Int
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

garbageHistogram = [(1,113),(2,30),(3,9),(4,23),(5,3),(7,1),(10,3)]
garbageTime = 4918

sampleHistogram :: MonadRandom m => [(a, Int)] -> m a 
sampleHistogram h = fmap (\v -> fromJust . snd . foldr iterate (v, Nothing) $ h) $ getRandomR (0, len - 1)
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


hurryUp :: Int -> Board -> Board
hurryUp n = V.modify (\v -> do
    let len = (20 - n) * 10
    MV.move (MV.slice 0 len v) (MV.slice (10 * n) len v)
    MV.set (MV.slice len (10 * n) v) (pack HurryUp))

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
          perfectLines = if (V.null . V.filter (\x -> x /= pack Empty) . board . gs $ s)
                            then 10
                            else 0
          atk = cboLines + clearedLines + perfectLines + attacks s

emptyBoard :: Board
emptyBoard = V.replicate 200 (pack Empty)

pieceQueue :: RandomGen g => g -> [Block]
pieceQueue = runIdentity . evalRandT (fmap mconcat . sequence . repeat . shuffleM $ [ I, J, L, O, S, T, Z ])

startingState :: RandomGen g => g -> SimulatorState
startingState g = SimulatorState (GameState emptyBoard (startingPosition active) Nothing queue []) leftOver 0 0
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

advanceBoard :: MonadRandom m => Int -> SimulatorState -> m SimulatorState
advanceBoard n state = join . fmap (queueGarbage . applyHurryUp n) . applyGarbage cleared . updateAttack cleared . setBoard brd $ state
    where (cleared, brd) = clearLines . board . gs $ state

toggleHold :: SimulatorState -> Maybe SimulatorState
toggleHold state = case held (gs state) of
                     Nothing -> cycleActive . setHeld hld $ state
                     Just k  -> Just . setActive (startingPosition k) . setHeld hld $ state
  where hld = Just . kind . active . gs $ state 

advance :: MonadRandom m => Int -> Action -> SimulatorState -> m (Maybe SimulatorState)
advance _ Hold s = pure (toggleHold s)
advance n HardDrop s = fmap cycleActive . advanceBoard n . setBoard (dropBlock (board . gs $ s) (active . gs $ s)) $ s
advance _ act s = pure . Just . setActive (moveBlock' (board . gs $ s)  act (active . gs $ s)) $ s

simulateAI :: (MonadIO m, RandomGen g) => g -> Int -> AIState -> m (Int, Int)
simulateAI gen ct = flip evalRandT g1 . evalStateT (go ct st0)
    where (g0, g1) = split gen
          st0 = startingState g0
          disp :: MonadIO m => SimulatorState -> m ()
          disp state = liftIO . (>> putStrLn "") . printBoard . addActiveBlock (board . gs $ state) . active . gs $ state
          go :: (MonadIO m, MonadRandom m) => Int -> SimulatorState -> StateT AIState m (Int, Int)
          go n st = do
              disp st
              liftIO . putStrLn $ "Combo: " ++ (show (combo st)) ++ " Total Attack: " ++ (show (attacks st)) ++ "\nIncoming: " ++ (show . garbage . gs  $ st) ++ "\n"
              actions <- runAI . gs $ st
              st' <- foldl (\st' act -> join . fmap (fmap join . sequence . fmap (advance n act)) $ st') (pure $ Just st) actions
              case st' of
                Just s -> go (n + 1) s
                Nothing -> pure (n, attacks st)
