-- The actual tetris AI.
-- Heavily inspired by Lee Yiyuan's AI (https://github.com/LeeYiyuan/tetrisai).
{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving #-}
module AI (AIState (AIState), defaultState, listToAI, runAI) where

import Control.Applicative
import Control.Monad.Logic
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Random
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Numeric.LinearAlgebra.Static (L, R, matrix, vector, headTail, dvmap, rand, (#>), (#), (&))

import Tetris

data AtkState = AtkState { combo :: Int, lines :: Int }

data AIState = AIState  { l1 :: (L 7 14)
                        , l2 :: (L 1 8)
                        , scombo :: Int 
                        }
  deriving Show

-- See https://codemyroad.wordpress.com/2013/04/14/tetris-ai-the-near-perfect-player/
defaultState :: IO AIState
defaultState = AIState <$> rand <*> rand <*> pure 0
    -- AIState (matrix [-0.510066, 0.760666, -0.35663, -0.184483]) 0

listToAI :: [Double] -> AIState
listToAI ws = AIState (matrix w1) (matrix w2) 0
    where (w1, w2) = splitAt (7 * 14) ws

runAI :: (MonadRandom m) => GameState -> StateT AIState m [Action]
runAI state = do
    options <- runComputation state (possible 2 >> score)
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
getAtk :: Computation m AtkState
getAtk = snd <$> Computation get
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

blId :: Block -> Int
blId I = 0
blId J = 1
blId L = 2
blId O = 3
blId S = 4
blId T = 5
blId Z = 6

score :: Monad m => Computation (StateT AIState m) (Double, Bool)
score = do
    state <- getState
    (AtkState cmbo lns) <- getAtk
    let cvrt :: Int -> Double
        cvrt = fromInteger . toInteger
        qvec :: R 7
        qvec = vector . V.toList . V.modify (\v -> sequence_ . fmap (MV.modify v (+ 1) . blId) . queue $ state) $ V.replicate 7 0 
        cl = completeLines . board $ state
        bvec :: R 4
        bvec = vector . fmap (\x -> cvrt . x . board $ state) $ [ aggregateHeight, const cl , holes, bumpiness ]
        v :: R 13
        v = qvec # bvec & cvrt cmbo & cvrt lns
        relu :: Double -> Double
        relu r 
          | r < -1 = -1 
          | r > 1  = 1
          | otherwise = r
    (AIState l1 l2 _) <- lift get
    pure . fmap (const (cl > 0)) . headTail $ l2 #> ((dvmap relu $ l1 #> (v & 1)) & 1)
