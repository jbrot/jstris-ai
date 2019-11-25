-- The actual tetris AI.
-- Heavily inspired by Lee Yiyuan's AI (https://github.com/LeeYiyuan/tetrisai).
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AI (AIState, defaultState, runAI) where

import Control.Applicative
import Control.Monad.Logic
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Random
import Data.List
import Data.Maybe

import Tetris

data AIState = AIState
  deriving Show

defaultState :: AIState
defaultState = AIState

runAI :: (MonadIO m, MonadRandom m) => GameState -> StateT AIState m [Action]
runAI state = do
    options <- runComputation state (possible 2 >> score')
    if null options
       then pure [ HardDrop ]
       else pure . (<> [ HardDrop ]) . takeWhile (/= HardDrop) . snd . maximumBy (\a b -> compare (fst a) (fst b)) $ options

newtype Computation m a = Computation { unComp :: StateT GameState (WriterT [Action] (LogicT m)) a}
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadTrans Computation where
    lift = Computation . lift . lift . lift

runComputation :: Monad m => GameState -> Computation m a -> m [(a, [Action])]
runComputation gs c = observeAllT . runWriterT . flip evalStateT gs . unComp $ c

getState :: Computation m GameState
getState = Computation get
putState :: GameState -> Computation m ()
putState = Computation . put
modifyState :: (GameState -> GameState) -> Computation m ()
modifyState = Computation . modify
tellAction :: Action -> Computation m ()
tellAction = Computation . lift . tell . (:[])

liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe = maybe mzero pure

act :: MonadRandom m => Action -> Computation m ()
act a = do
    state <- getState
    state' <- liftMaybe . moveActive a $ state
    tellAction a
    putState state'
    when (a == HardDrop) $ do
        let (ct, state'') = clearLines' . addActive $ state'
        putState  (reduceGarbage ct state'')
        when (ct == 0) (putState =<< lift . addGarbage =<< getState)
        modifyState (\g@GameState{queue = h:t} -> g{active = startingPosition h, queue = t})
        fstate <- getState
        guard (canAddActiveBlock (board fstate) (active fstate))

rotations :: MonadRandom m => Computation m ()
rotations = go 3
  where go 0 = pure ()
        go n = pure () `mplus` (act RotateRight >> go (n - 1))

translations :: MonadRandom m => Computation m ()
translations = pure() `mplus` go MoveLeft `mplus` go MoveRight
    where go a = act a >> (pure () `mplus` go a)

possible :: MonadRandom m => Int -> Computation m ()
possible 0 = pure ()
possible n = rotations >> translations >> act HardDrop >> possible (n - 1)

aggregateHeight :: Board -> Int
aggregateHeight board = sum (height board <$> [0..9])

height :: Board -> Col -> Int
height board c = (20 -) . head . (<> [20]) . filter (\r -> getSquare (r,c) board /= (pack Empty)) $ [0..19]

completeLines :: Board -> Int
completeLines board = length . filter (complete board) $ [0..19]

holes :: Board -> Int
holes board = sum (colHoles <$> [0..9])
    where colHoles :: Col -> Int
          colHoles c = length . filter (\r -> r > (20 - height board c) && getSquare (r,c) board == (pack Empty)) $ [0..19]

bumpiness :: Board -> Int
bumpiness board = sum . fmap (\c -> abs (height board c - height board (c + 1))) $ [0..8]

-- See https://codemyroad.wordpress.com/2013/04/14/tetris-ai-the-near-perfect-player/
score :: Board -> Float
score board = (-0.510066 * itf (aggregateHeight board)) + (0.760666 * itf (completeLines board)) + (-0.35663 * itf (holes board)) + (-0.184483 * itf (bumpiness board))
  where itf = fromInteger . toInteger

score' :: Computation m Float
score' = fmap (score . board) getState
