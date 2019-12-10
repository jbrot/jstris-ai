{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module AI (NL, NNet, AIState (AIState), nn, defaultState, parseAI, saveAI, stepAI, runAI) where

import Control.Monad.Random
import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import Grenade
import System.Clock

import Tetris.Action
import Tetris.Block
import Tetris.Board
import Tetris.State
import MCTS

-- Input: Board (200) + Queue (7 * 5 = 35) + Active (7) + Active Position (2) + Active Rotation (1) + Combo (1) + Incoming (1) = 247
-- Output: Left | Right | Rotate Left | Rotate Right | Drop (5)
type NL = '[ FullyConnected 247 1024, Relu, FullyConnected 1024 5, Softmax ]
type NNet = Network NL '[ 'D1 247, 'D1 1024, 'D1 1024, 'D1 5, 'D1 5 ]

data AIState = AIState  { tree :: Maybe (MCTree TransitionState)
                        }

params :: MCTS
params = MCTS { linesToReward = fromInteger . toInteger
              , stateToReward = const 0
              , simulate = pure . (+ 1) . (/ 100) . realToFrac . score . board
              , lossReward = -10 
              , gamma = 1
              , cp = 1 / sqrt 2
              }

defaultState :: IO AIState
defaultState = pure (AIState Nothing)

nn :: AIState -> NNet
nn = undefined

parseAI :: ByteString -> Either String AIState
parseAI = undefined

saveAI :: AIState -> ByteString
saveAI = undefined

stepAI :: (MonadRandom m, MonadIO m) => GameState -> StateT AIState m (Action, Gradients NL)
stepAI = undefined

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 f = f
iterateM n f = f >=> iterateM (n - 1) f

iterateUntil :: MonadIO m => TimeSpec -> (a -> m a) -> a -> m a
iterateUntil t f a = do
    a' <- iterateM 100 f a
    now <- liftIO $ getTime Monotonic
    if now > t
       then pure a'
       else iterateUntil t f a'

runAI :: (MonadRandom m, MonadIO m) => Int -> GameState -> StateT AIState m [(Action, Maybe (Gradients NL))]
runAI _ gs = do
    oldT <- fmap tree get
    time <- liftIO $ getTime Monotonic
    newT <- iterateUntil (time + (TimeSpec 0 $ 100 * 1000 * 1000)) (fmap snd . rollout params) (newRootNode oldT gs)
    (choice, leftover) <- decide newT
    put (AIState leftover)
    pure $ fmap (\a -> (a, Nothing)) choice

aggregateHeight :: Board -> Int
aggregateHeight board = sum (height board <$> [0..9])

height :: Board -> Col -> Int
height board c = (20 -) . head . (<> [20]) . filter (\r -> getSquare (r,c) board /= Empty) $ [0..19]

completeLines :: Board -> Int
completeLines board = length . filter (complete board) $ [0..19]

holes :: Board -> Int
holes board = sum (colHoles <$> [0..9])
    where colHoles :: Col -> Int
          colHoles c = length . filter (\r -> r > (20 - height board c) && getSquare (r,c) board == Empty) $ [0..19]

bumpiness :: Board -> Int
bumpiness board = sum . fmap (\c -> abs (height board c - height board (c + 1))) $ [0..8]

-- See https://codemyroad.wordpress.com/2013/04/14/tetris-ai-the-near-perfect-player/
score :: Board -> Float
score board = (-0.510066 * itf (aggregateHeight board)) + (0.760666 * itf (completeLines board)) + (-0.35663 * itf (holes board)) + (-0.184483 * itf (bumpiness board))
  where itf = fromInteger . toInteger
