-- The actual tetris AI.
-- Heavily inspired by Lee Yiyuan's AI (https://github.com/LeeYiyuan/tetrisai).
{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving #-}
module AI (AIState (AIState), defaultState, runAI) where

import Control.Applicative
import Control.Monad.Logic
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Random
import Data.List
import Data.Maybe
import Numeric.LinearAlgebra.Static (L, matrix, vector, headTail, (#>))

import Tetris

data AtkState = AtkState { combo :: Int, lines :: Int }

data AIState = AIState  { l1 :: (L 1 4)
                        , scombo :: Int 
                        }
--     { l1 :: L 7 1
--                        , l2 :: L 7 13
  deriving Show

-- See https://codemyroad.wordpress.com/2013/04/14/tetris-ai-the-near-perfect-player/
defaultState :: AIState
defaultState = AIState (matrix [-0.510066, 0.760666, -0.35663, -0.184483]) 0

runAI :: (MonadRandom m) => GameState -> StateT AIState m [Action]
runAI state = do
    -- modify (\st -> st{attack = 0})
    options <- runComputation state (possible 2 >> score')
    if null options
       then pure [ HardDrop ]
       else do
           let ((mx, cp), as) = maximumBy (\a b -> compare (fst $ fst a) (fst $ fst b)) options
           modify (\st -> st{scombo = if cp then 1 + scombo st else 0})
           pure . (<> [ HardDrop ]) . takeWhile (/= HardDrop) $ as

newtype Computation m a = Computation { unComp :: StateT (GameState, AtkState) (WriterT [Action] (LogicT m)) a}
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadTrans Computation where
    lift = Computation . lift . lift . lift

runComputation :: Monad m => GameState -> Computation (StateT AIState m) a -> StateT AIState m [(a, [Action])]
runComputation gs c = do
    st <- get
    observeAllT . runWriterT . flip evalStateT (gs, AtkState (scombo st) 0) . unComp $ c

getState :: Computation m GameState
getState = fst <$> Computation get
putState :: GameState -> Computation m ()
putState st = Computation $ modify (\(_,b) -> (st,b))
modifyState :: (GameState -> GameState) -> Computation m ()
modifyState f = Computation $ modify (\(s,b) -> (f s,b))
modifyAtk :: (AtkState -> AtkState) -> Computation m ()
modifyAtk = Computation . modify . fmap
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
        modifyAtk (\(AtkState c l) -> AtkState (c + 1) l)
        when (ct == 0) $ do
            putState =<< lift . addGarbage =<< getState
            modifyAtk (\(AtkState _ l) -> AtkState 0 l)
        modifyAtk (\(AtkState c l) -> AtkState c (l + attackLines (board state'') c ct))
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

score :: Monad m => Board -> StateT AIState m (Double, Bool)
score board = do
    let cl = completeLines board
        v = vector . fmap (\x -> fromInteger . toInteger . x $ board) $ [ aggregateHeight, const cl , holes, bumpiness ]
    (AIState weights _) <- get
    pure . fmap (const (cl > 0)) . headTail $ weights #> v

score' :: Monad m => Computation (StateT AIState m) (Double, Bool)
score' = (lift . score . board) =<< getState
