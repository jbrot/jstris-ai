{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving  #-}
module MCTS ( Choice, Reward
            , MCTS (..), NodeInfo (..), MCTree (..)
            , rollout, rootNode, decide, newRootNode
            ) where


import Control.Applicative
import Control.Monad.Logic
import Control.Monad.Random
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Vector (Vector)
import qualified Data.Vector as V

import Tetris.Action
import Tetris.Simulator
import Tetris.State

type Choice = [Action]
type Reward = Double

-- Parameters for configuring the MCTS search.
data MCTS = MCTS { linesToReward :: AttackLines -> Reward
                 , stateToReward :: GameState -> Reward
                 , lossReward :: Reward
                 , gamma :: Double
                 , cp :: Double -- Exploration/exploitation factor (usually 1 / sqrt(2))
                 }

data NodeInfo = NodeInfo { q :: Reward -- Total child reward from rollouts
                         , r :: Reward -- Reward at this node
                         , n :: Double -- Number of visits
                         }

data MCTree a where
    StateNode :: NodeInfo -> GameState -> Vector (Choice, Maybe (MCTree TransitionState)) -> MCTree GameState
    TransitionNode :: NodeInfo -> TransitionState -> Map GameState (MCTree GameState) -> MCTree TransitionState

-- Start a new MCTree
rootNode :: GameState -> MCTree GameState
rootNode gs = StateNode (NodeInfo 0 0 0) gs (moves gs)

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
rollout params (StateNode (NodeInfo q r n) gs opts) = do
    index <- bestMoveIndex (uctScore params n . snd) opts
    (rwd, entry) <- descendState params gs (opts V.! index)
    pure $ (r + (gamma params) * rwd, StateNode (NodeInfo (q + rwd) r (n + 1)) gs (opts V.// [(index, entry)]))

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
choiceScore (Just (TransitionNode info _ _)) = r info + (q info) / (n info)
choiceScore _ = -1 / 0

descendState :: MonadRandom m => MCTS -> GameState -> (Choice, Maybe (MCTree TransitionState)) -> m (Reward, (Choice, Maybe (MCTree TransitionState)))
descendState params st (c, mts) = fmap (fmap $ \s -> (c, Just s)) . descendTransition params . fromMaybe (applyActions params c st 0) $ mts

applyActions :: MCTS -> [Action] -> GameState -> AttackLines -> MCTree TransitionState
applyActions _ [] _ _ = undefined
applyActions params (a:as) gs c = case applyAction a gs of
                                    (c2, Right gs2) -> applyActions params as gs2 (c + c2)
                                    (c2, Left ts)   -> TransitionNode (NodeInfo 0 (linesToReward params $ c + c2) 0) ts M.empty

descendTransition :: MonadRandom m => MCTS -> MCTree TransitionState -> m (Reward, MCTree TransitionState)
descendTransition params (TransitionNode info ts children) = monteCarloTransition ts >>= \mgs ->
    case mgs of
      Nothing -> pure (lossReward params, TransitionNode info{n = 1 + n info} ts children) -- We lose
      Just gs -> do
          (children', Sum result) <- runWriterT $ M.alterF (rolloutTransition params gs) gs children
          pure (r info + result, TransitionNode info{q = result + q info, n = 1 + n info} ts children')

rolloutTransition :: MonadRandom m => MCTS -> GameState -> Maybe (MCTree GameState) -> WriterT (Sum Reward) m (Maybe (MCTree GameState))
rolloutTransition params _ (Just node) = do
    -- We're not at a leaf, so we keep descending
    (rwd, node') <- lift (rollout params node)
    tell (Sum rwd)
    pure (Just node')
rolloutTransition params gs Nothing = do
    -- We're at a leaf, create a new node.
    rwd <- lift (simulate params gs)
    tell (Sum rwd)
    pure . Just $ StateNode (NodeInfo rwd (stateToReward params gs) 1) gs (moves gs)

-- Play out uniformly randomly from this state
simulate :: MonadRandom m => MCTS -> GameState -> m Reward
simulate params = go 20
  where go :: MonadRandom m => Int -> GameState -> m Reward
        go 0  _ = pure 0
        go n gs = do
            let poss = moves gs
            choice <- fmap (fst . (poss V.!)) $ getRandomR (0, length poss - 1)
            let (TransitionNode (NodeInfo _ rwd _) ts _) = applyActions params choice gs 0
            mgs <- monteCarloTransition ts
            case mgs of
              Nothing -> pure (rwd + lossReward params)
              Just gs' -> fmap (\r -> rwd + stateToReward params gs' + (gamma params) * r) (go (n - 1) gs')


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

act :: Action -> Computation ()
act a = do
    state <- getState
    state' <- liftMaybe . moveActive a $ state
    tellAction a
    putState state'

rotations :: Computation ()
rotations = go 3
  where go 0 = pure ()
        go n = pure () `mplus` (act RotateRight >> go (n - 1))

translations :: Computation ()
translations = pure() `mplus` go MoveLeft `mplus` go MoveRight
    where go a = act a >> (pure () `mplus` go a)

possible :: Computation ()
possible = rotations >> translations
