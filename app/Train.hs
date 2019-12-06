module Train where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Maybe (fromJust, isNothing)
import Grenade
import System.Random
import Text.Printf

import AI
import Grenade.Exts
import Simulator
import Tetris
import Tetris.Board

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
updateNet st = st{rollout = [], adam = ad', as = AIState nn' 0}
  where gamma = 0.9
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

nextEp :: TState -> IO TState
nextEp ts = when (ep `mod` 10 == 0) logStat >> (resetSim . updateNet) ts{episode = ep, avg = navg}
    where reward = sum . fmap fst . rollout $ ts
          ep = 1 + episode ts
          navg = 0.05 * reward + 0.95 * (avg ts)
          logStat = printf "Episode: %d Last reward: %.02f Average: %.02f\n" ep reward navg

step :: Bool -> TState -> IO TState
step v ts = do
    when v . (>> putStrLn "") . printBoard . addActiveBlock (board . gs . ss $ ts) . active . gs . ss $ ts
    ((act, grad), as') <- runStateT (stepAI (gs . ss $ ts)) (as ts)
    act <- if kp ts == 10 then pure HardDrop else pure act
    nxt <- advance (stp ts) act (ss ts)
    if isNothing nxt
       then nextEp ts{rollout = (0,grad):(rollout ts)}
       else let (ss', atk) = fromJust nxt
                stp' = 1 + (stp ts)
                hd  = act == HardDrop
                kp' = if hd then 0 else 1 + (kp ts)
                adjRwd = realToFrac $ (kp ts - 5)^2
                rwd = 10 * (realToFrac atk) + (if hd then 1 / max adjRwd 0.5 else 0.1)
                rl' = (rwd, grad):(rollout ts)
             in pure ts{ss=ss', as=as', stp=stp', kp=kp', rollout=rl'}


runTraining :: Adam (Gradients NL) -> Bool -> IO ()
runTraining a v = go =<< TState <$> fmap startingState getStdGen <*> defaultState <*> pure 0 <*> pure 0 <*> pure [] <*>  pure a <*> pure 0 <*> pure 0
    where go :: TState -> IO ()
          go = step v >=> go
