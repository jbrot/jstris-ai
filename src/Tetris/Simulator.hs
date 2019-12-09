module Tetris.Simulator ( AttackLines, applyAction, monteCarloTransition
                        , SimulatorState(..), startingState, advance
                        ) where

import Control.Monad.Identity
import Control.Monad.Random
import Data.Bits
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed.Sized as U
import System.Random.Shuffle

import Tetris.Action
import Tetris.Block
import Tetris.Board
import Tetris.State

type AttackLines = Int

-- Board -> Combo -> Cleared -> LinesSent
-- Combo counts consecutive clears, so if cleared > 0, then combo >= 1.
attackLines :: Board -> Int -> Int -> AttackLines
attackLines board combo cleared = cboLines + clearedLines
    where cboLines = case (combo - 1) of
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
                       _ -> 5
          mask = 1023 `shiftL` 8
          clearedLines = if U.all (\r -> r .&. mask == 0) (snd board)
                            then 10
                            else case cleared of 
                                    0 -> 0
                                    1 -> 0
                                    2 -> 1
                                    3 -> 2
                                    4 -> 4
                                    _ -> undefined

cycleActive :: GameState -> GameState
cycleActive gs@GameState{queue = q:qs} = gs{active = startingPosition q, queue = qs}
cycleActive g = g

garbageHistogram = [(1,517),(2,111),(3,27),(4,52),(5,16),(7,1),(10,3)]
garbageTime = 10081

sampleHistogram :: MonadRandom m => [(a, Int)] -> m a 
sampleHistogram h = fmap (\v -> fromJust . snd . foldl (flip iterate) (v, Nothing) $ h) $ getRandomR (0, len - 1)
    where len = sum . fmap snd $ h
          iterate :: (a, Int) -> (Int, Maybe a) -> (Int, Maybe a)
          iterate _ (_, Just a) = (0, Just a)
          iterate (a, c) (r, Nothing) = if c >= r then (0, Just a)
                                                  else (r - c, Nothing)

queueGarbage :: MonadRandom m => GameState -> m GameState 
queueGarbage s = getRandomR (0, garbageTime) >>= \r -> 
    if r > (sum . fmap snd $ garbageHistogram)
       then pure s
       else do
           ct <- sampleHistogram garbageHistogram
           queueGarbage s{garbage = garbage s <> [ct]}

-- Compute the GameState after the specified action is applied. If computing the new
-- state is entirely deterministic, this returns Right GameState with the new state.
-- If the new state is probablistic, this returns Left TransitionState, where the
-- TransitionState contains the deterministic updates. You can then apply the
-- probabilistic update via other functions.
--
-- Things that need to be done for a TransitionState:
--    1) Add a new Block to the end of the queue
--    2) If the first component is True, spawn queued garbage
--    3) Possibly queue new garbage
--
-- Note that a TransitionState is reached either when a HardDrop occurs or on the
-- first Hold. This means that we will get one additional garbage spawn on the first
-- Hold which I find acceptable.
applyAction :: Action -> GameState -> (AttackLines, Either TransitionState GameState)
applyAction Hold gs = (,) 0 $ if canHold gs
                                then case held gs of
                                       Nothing -> Left . TransitionState $ (False, cycleActive gs{held = Just . kind . active $ gs})
                                       Just k  -> Right gs{held = Just . kind . active $ gs, active = startingPosition k}
                                else Right gs
applyAction HardDrop gs = (atk, Left . TransitionState $ (cl == 0, reduceGarbage cl gs1))
    where (cl, gs1) = clearLines' . cycleActive . addActive . moveActive' HardDrop $ gs
          combo' = if cl > 0 then 1 + combo gs1 else 0
          atk = attackLines (board gs1) combo' cl
applyAction act gs = ((,) 0) . Right . moveActive' act $ gs


transitionWithBlockHU :: MonadRandom m => TransitionState -> Int -> Block -> m (Maybe GameState)
transitionWithBlockHU (TransitionState (grb, gs0)) _ b = do
    gs1 <- if grb then addGarbage gs0 else pure gs0
    gs2 <- queueGarbage gs1
    let gs3 = gs2{board = hurryUp 0 (board gs2), queue = (queue gs2) <> [b]}
    if canAddActiveBlock (board gs3) (active gs3)
       then pure (Just gs3)
       else pure Nothing

-- Transition with randomly dealt garbage and next piece.
-- Returns Nothing if the game is over.
monteCarloTransition :: MonadRandom m => TransitionState -> m (Maybe GameState)
monteCarloTransition st = transitionWithBlockHU st 0 L -- =<< fmap toEnum (getRandomR (0,6))

data SimulatorState = SimulatorState { gs :: GameState
                                     , squeue :: [Block]
                                     }

pieceQueue :: RandomGen g => g -> [Block]
pieceQueue = runIdentity . evalRandT (fmap mconcat . sequence . repeat . shuffleM $ [ I, J, L, O, S, T, Z ])

startingState :: RandomGen g => g -> SimulatorState
startingState g = SimulatorState (GameState emptyBoard (startingPosition active) Nothing True 0 queue []) leftOver
    where (active:queue, leftOver) = splitAt 6 . pieceQueue $ g

hurryUpCount :: Int -> Int
hurryUpCount n
  | n < 900 = 0
  | n `mod` 20 == 0 = 1
  | otherwise = 0

-- Transition according to the extra information in the SimulatorState.
simulatorStateTransition :: MonadRandom m => TransitionState -> Int -> SimulatorState ->  m (Maybe SimulatorState)
simulatorStateTransition ts step ss@SimulatorState{ squeue = q} = fmap (fmap newState) $ transitionWithBlockHU ts (hurryUpCount step) (head q)
  where newState gs = ss{gs = gs, squeue = tail q}

advance :: MonadRandom m => Int -> Action -> SimulatorState -> m (Maybe (SimulatorState, AttackLines))
advance step act ss = case res of
                     Right gs -> pure . Just $ (ss{gs = gs}, atk)
                     Left  ts -> fmap (fmap (\ss -> (ss, atk))) $ simulatorStateTransition ts step ss
  where (atk, res) = applyAction act (gs ss)
