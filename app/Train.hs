module Train where

import Control.Monad
import Control.Monad.Trans.State.Strict
import qualified Data.ByteString  as B
import Data.Maybe (fromJust, isNothing)
import Grenade
import System.Random
import Text.Printf

import AI
import Grenade.Exts
import Tetris.Action
import Tetris.Block
import Tetris.Board
import Tetris.Simulator
import Tetris.State

data TState = TState { ss :: SimulatorState
                     , as :: AIState
                     , stp :: Int
                     , kp :: Int
                     , rollout :: [(Float, Gradients NL)]
                     , adam :: Adam (Gradients NL)
                     , episode :: Int
                     , avg :: Float
                     }

resetSim :: TState -> IO TState
resetSim ts = fmap (\g -> ts{ss = startingState g, stp = 0, kp = 0}) getStdGen

updateNet :: TState -> TState
updateNet st = st{rollout = [], adam = ad', as = AIState Nothing}
  where gamma = 0.95
        (_,gtrl) = foldl (\(v,ls) (r,g) -> let nv = gamma * v + r in (nv, (nv,g):ls)) (0, []) (rollout st)
        average :: Fractional n => [n] -> n
        average = (/) <$> sum <*> (realToFrac . length)
        avg :: Float
        avg = average (fmap fst gtrl) 
        stdev :: Float
        stdev = sqrt . average . fmap (\(x,_) -> (x - avg)^2) $ gtrl
        rtf = realToFrac
        upd = foldr (\(x,g) ag -> ag + ((negate . rtf $ (x - avg) / (stdev + 1e-9)) * g)) (rtf 0) gtrl
        (ad', nn') = runAdam (adam st) upd (nn . as $st)

nextEp :: Maybe FilePath -> TState -> IO TState
nextEp fp ts = when (ep `mod` 10 == 0) logStat >> (resetSim . updateNet) ts{episode = ep, avg = navg}
    where reward = sum . fmap fst . rollout $ ts
          ep = 1 + episode ts
          navg = 0.05 * reward + 0.95 * (avg ts)
          logStat = do
              printf "Episode: %d Last reward: %.02f Average: %.02f\n" ep reward navg
              void . sequence . fmap (\afp -> B.writeFile afp . saveAI . as $ ts) $ fp

step :: Bool -> Maybe FilePath -> TState -> IO TState
step v fp ts = do
    when v . (>> putStrLn "") . printBoard . addActiveBlock (board . gs . ss $ ts) . active . gs . ss $ ts
    ((act, grad), as') <- runStateT (stepAI (gs . ss $ ts)) (as ts)
    act <- if kp ts == 10 then pure HardDrop else pure act
    nxt <- advance (stp ts) act (ss ts)
    if isNothing nxt
       then nextEp fp ts{rollout = (0,grad):(rollout ts)}
       else let (ss', atk) = fromJust nxt
                hd  = act == HardDrop
                stp' = stp ts + (if hd then 1 else 0)
                kp' = if hd then 0 else 1 + (kp ts)
                rwd = 10 * (realToFrac atk) + if hd then (score . board . gs . ss $ ts) else 0
                rl' = (rwd, grad):(rollout ts)
             in pure ts{ss=ss', as=as', stp=stp', kp=kp', rollout=rl'}


runTraining :: Adam (Gradients NL) -> AIState -> Bool -> Maybe FilePath -> IO ()
runTraining ad a v f = go =<< TState <$> fmap startingState getStdGen <*> pure a <*> pure 0 <*> pure 0 <*> pure [] <*>  pure ad <*> pure 0 <*> pure 0
    where go :: TState -> IO ()
          go = step v f >=> go

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
