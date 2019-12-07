{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TestMC (monteCarloStep) where

import Control.Applicative
import Control.Monad.Logic
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Random
import qualified Data.Vector as V

import Tetris.Action
import Tetris.Block
import Tetris.Board
import Tetris.State

monteCarloStep :: (MonadIO m, MonadRandom m) => GameState -> m [Action]
monteCarloStep state = do
    options <- fmap V.fromList $ runComputation state possible
    if V.null options
       then pure [ HardDrop ]
       else fmap ((<> [ HardDrop ]) . snd . (V.!) options) (getRandomR (0, V.length options - 1))

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

possible :: MonadRandom m => Computation m ()
possible = rotations >> translations
