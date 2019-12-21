{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables  #-}
module MCTS ( Choice, Reward
            , MCTS (..), NodeInfo (..), MCTree (..)
            , rollout, rootNode, decide, newRootNode
            , simulateU, simulateA
            ) where


import Control.Applicative
import Control.Monad.Logic
import Control.Monad.Random
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS
import Data.List (maximumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Sized as U

import Tetris.Action
import Tetris.Block
import Tetris.Board
import Tetris.Simulator
import Tetris.State

type Choice = [Action]
type Reward = Double

-- Parameters for configuring the MCTS search.
data MCTS = MCTS { linesToReward :: AttackLines -> Reward
                 , stateToReward :: GameState -> Reward
                 , simulate :: (forall m. MonadRandom m => GameState -> m Reward) -- Estimate reward at a given position
                 , lossReward :: Reward
                 , gamma :: Double
                 , cp :: Double -- Exploration/exploitation factor (usually 1 / sqrt(2))
                 }

data NodeInfo = NodeInfo { q :: Reward -- Total child reward from rollouts
                         , r :: Reward -- Reward at this node
                         , n :: Double -- Number of visits
                         , best :: Reward
                         }

addReward :: Reward -> NodeInfo -> NodeInfo
addReward rwd (NodeInfo q r n b) = NodeInfo (q + rwd) r (n + 1) (max rwd b)

data MCTree a where
    StateNode :: NodeInfo -> GameState -> Vector (Choice, Maybe (MCTree TransitionState)) -> MCTree GameState
    TransitionNode :: NodeInfo -> TransitionState -> Map GameState (MCTree GameState) -> MCTree TransitionState

-- Start a new MCTree
rootNode :: GameState -> MCTree GameState
rootNode gs = StateNode (NodeInfo 0 0 0 (-1 / 0)) gs (moves gs)

-- Pick the best option currently in the MCTree, and get the portion of the tree below it.
decide :: MonadRandom m => MCTree GameState -> m (Choice, Maybe (MCTree TransitionState))
decide (StateNode _ _ opts) = fmap (opts V.!) (bestMoveIndex (choiceScore . snd) opts)

-- Given a portion of the tree from `decide`, and the resulting new GameState after taking
-- the provided action, reduce the tree again to the appropriate branch, which can then
-- be passed to rollout to begin searching again.
newRootNode :: Maybe (MCTree TransitionState) -> GameState -> MCTree GameState
newRootNode Nothing gs = rootNode gs
newRootNode (Just (TransitionNode _ _ map)) gs = M.findWithDefault (rootNode gs) gs map

-- Perform one step of UCT Monte Carlo Tree Search.
-- That is, we keep picking the best move available until we reach an unexplored node.
-- Then, we add that node to the tree and do a uniform roll out from it to give it a default value.
rollout :: MonadRandom m => MCTS -> MCTree GameState -> m (Reward, MCTree GameState)
rollout params (StateNode info  gs opts) = do
    index <- bestMoveIndex (uctScore params (n info) . snd) opts
    (rwd, entry) <- descendState params gs (opts V.! index)
    pure $ (r info + (gamma params) * rwd, StateNode (addReward rwd info) gs (opts V.// [(index, entry)]))

bestMoveIndex :: MonadRandom m => (a -> Double) -> Vector a -> m Int
bestMoveIndex score opts = fmap (bestIndices V.!) $ getRandomR (0, length bestIndices - 1)
    where scores = fmap score opts
          best = V.maximum scores
          bestIndices = V.findIndices (== best) scores

-- Parent Total -> Child -> Score
uctScore :: MCTS -> Double -> Maybe (MCTree TransitionState) -> Double
uctScore params total (Just (TransitionNode info _ _)) = r info + (q info) / (n info) + (cp params) * sqrt ((log total) / (n info))
uctScore _ _ _ = 1 / 0 -- Infinity

choiceScore :: Maybe (MCTree TransitionState) -> Double
choiceScore (Just (TransitionNode info _ _)) = r info + best info
choiceScore _ = -1 / 0

descendState :: MonadRandom m => MCTS -> GameState -> (Choice, Maybe (MCTree TransitionState)) -> m (Reward, (Choice, Maybe (MCTree TransitionState)))
descendState params st (c, mts) = fmap (fmap $ \s -> (c, Just s)) . descendTransition params . fromMaybe (applyActions params c st 0) $ mts

applyActions :: MCTS -> [Action] -> GameState -> AttackLines -> MCTree TransitionState
applyActions _ [] _ _ = undefined
applyActions params (a:as) gs c = case applyAction a gs of
                                    (c2, Right gs2) -> applyActions params as gs2 (c + c2)
                                    (c2, Left ts)   -> TransitionNode (NodeInfo 0 (linesToReward params $ c + c2) 0 (-1 / 0)) ts M.empty

descendTransition :: MonadRandom m => MCTS -> MCTree TransitionState -> m (Reward, MCTree TransitionState)
descendTransition params (TransitionNode info ts children) = monteCarloTransition ts >>= \mgs ->
    case mgs of
      Nothing -> pure (r info + lossReward params, TransitionNode (addReward (lossReward params) info) ts children) -- We lose
      Just gs -> do
          (children', Sum result) <- runWriterT $ M.alterF (rolloutTransition params gs) gs children
          pure (r info + result, TransitionNode (addReward result info) ts children')

rolloutTransition :: MonadRandom m => MCTS -> GameState -> Maybe (MCTree GameState) -> WriterT (Sum Reward) m (Maybe (MCTree GameState))
rolloutTransition params gs (Just (StateNode i _ m)) = do
    -- We're not at a leaf, so we keep descending
    (rwd, node') <- lift (rollout params (StateNode i gs m))
    tell (Sum rwd)
    pure (Just node')
rolloutTransition params gs Nothing = do
    -- We're at a leaf, create a new node.
    rwd <- lift $ simulate params gs
    tell . Sum  $ stateToReward params gs + (gamma params) * rwd
    pure . Just $ StateNode (NodeInfo rwd (stateToReward params gs) 1 rwd) gs (moves gs)

-- Play out n moves uniformly randomly from this state
simulateU :: (MonadRandom m) => MCTS -> Int -> GameState -> m Reward
simulateU _ 0 _ = pure 0
simulateU params n gs = do
    let poss = moves gs
    choice <- fmap (fst . (poss V.!)) $ getRandomR (0, length poss - 1)
    let (TransitionNode (NodeInfo _ rwd _ _) ts _) = applyActions params choice gs 0
    mgs <- monteCarloTransition ts
    case mgs of
        Nothing -> pure (rwd + lossReward params)
        Just gs' -> fmap (\r -> rwd + stateToReward params gs' + (gamma params) * r) (simulateU params (n - 1) gs')

-- Play out according to the old AI.
simulateA :: (MonadRandom m) => MCTS -> Int -> GameState -> m Reward
simulateA _ 0 _ = pure 0
simulateA params n gs = do
    let poss = runComputation gs (possible >> act HardDrop >> score')
        (_, choice) = maximumBy (\(s1,_) (s2,_) -> compare s1 s2) poss
    let (TransitionNode (NodeInfo _ rwd _ _) ts _) = applyActions params choice gs 0
    mgs <- monteCarloTransition ts
    case mgs of
        Nothing -> pure (rwd + lossReward params)
        Just gs' -> fmap (\r -> rwd + stateToReward params gs' + (gamma params) * r) (simulateA params (n - 1) gs')

moves :: GameState -> Vector (Choice, Maybe (MCTree TransitionState))
moves = V.fromList . fmap (\(_,c) -> (c <> [HardDrop], Nothing)) . flip runComputation possible

newtype Computation a = Computation { unComp :: StateT GameState (WriterT [Action] Logic) a}
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

runComputation :: GameState -> Computation a -> [(a, [Action])]
runComputation gs c = observeAll . runWriterT . flip evalStateT gs . unComp $ c

getState :: Computation GameState
getState = Computation get
putState :: GameState -> Computation ()
putState = Computation . put
tellAction :: Action -> Computation ()
tellAction = Computation . lift . tell . (:[])

liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe = maybe mzero pure

act :: Action -> Computation AttackLines
act HardDrop = do
    state <- getState
    let (lines, res) = applyAction HardDrop state
    state' <- case res of
                Left trans -> liftMaybe . deterministicTransition $ trans
                Right _ -> undefined
    tellAction HardDrop
    putState state'
    pure lines
act a = do
    state <- getState
    state' <- liftMaybe . moveActive a $ state
    tellAction a
    putState state'
    pure 0

rotations :: Computation ()
rotations = go 3
  where go 0 = pure ()
        go n = pure () `mplus` (act RotateRight >> go (n - 1))

translations :: Computation ()
translations = pure() `mplus` go MoveLeft `mplus` go MoveRight
    where go a = act a >> (pure () `mplus` go a)

possible :: Computation ()
possible = rotations >> translations

aggregateHeight :: Board -> Int
aggregateHeight board = sum (height board <$> [0..9])

height :: Board -> Col -> Int
height board = fromInteger . toInteger . U.unsafeIndex (colHeights board)

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

score' :: Computation Float
score' = fmap (score . board) getState 
