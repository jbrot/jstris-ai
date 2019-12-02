{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Random.Strict
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.Text (Text)
import qualified Data.Vector.Unboxed as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isNothing)
import Grenade
import System.Random
import Test.WebDriver
import Test.WebDriver.Commands.Wait
import Test.WebDriver.JSON (ignoreReturn)
import Text.Printf

import AI
import CLI
import Grenade.Exts
import Parse
import Simulator
import Tetris

main :: IO ()
main = do
    cmd <- processCLI
    case cmd of
      Run a u -> parseAISpec a >>= \a' -> runOnline a' u
      Simulate a v -> parseAISpec a >>= \a' -> runSimulation a' v
      Train a v -> runTraining a v

-------------------------
-------------------------
-- | Run Online Code | --
-------------------------
-------------------------

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

inGame :: WD Bool
inGame = not <$> executeJS [] "return window.game == null || window.game.gameEnded"

waitForGameStart :: WD ()
waitForGameStart = waitUntil' 10000 600 $ do
    expect =<< inGame

-- Given a frame index, waits for a higher frame index and then retrieves the associated frame.
nextState :: Int -> WD (Int, GameState, [[Int]])
nextState curr = waitUntil' 10000 600 $ do
    count <- executeJS [] "return window.fcount;"
    expect $ count > curr

    (matrix, act, hld, q, inc) <- executeJS [] "return [window.game.matrix, window.game.activeBlock, window.game.blockInHold, window.game.queue, window.game.incomingGarbage]"
    let brd = V.map (pack . (colorsToSquare M.!)) . mconcat $ matrix
        garbage = fmap head $ inc
        gs = GameState brd act (kind <$> hld) (kind <$> q) garbage
    pure (count, gs, inc)

type Lines = Map Int Int
type Histogram = Map Int Int

mainLoop :: AIState -> WD (Lines, Int)
mainLoop = fmap fst . evalRandTIO . runStateT (go (0, (M.empty, 0)))
    where go :: (Int, (Lines, Int)) -> StateT AIState (RandT StdGen WD) (Lines, Int)
          go = guardInGame $ \(curr, (h, pct)) -> do
              (curr', state, inc) <- lift . lift . nextState $ curr
              let h' = foldr id h . fmap (\[size, id] -> M.insertWith (flip const) id size) $ inc
              -- liftIO . printBoard . addActiveBlock (board state) . active $ state 

              keys <- runAI 10 state
              -- liftIO . putStrLn $ "Keys: " <> show keys

              body <- lift . lift . findElem $ ByTag "body"
              lift . lift . sendKeys (mconcat . fmap (actionToText . fst) $ keys) $ body

              go (curr', (h', pct + 1))
          guardInGame :: ((Int, (Lines, Int)) -> StateT AIState (RandT StdGen WD) (Lines, Int)) -> (Int, (Lines, Int)) -> StateT AIState (RandT StdGen WD) (Lines, Int)
          guardInGame act p = do
              running <- lift (lift inGame)
              if running
                 then act p
                 else pure . snd $ p

updateHist :: Histogram -> Lines -> Histogram
updateHist = M.foldr (\v -> M.insertWith (+) v 1)

runOnline :: AIState -> String -> IO ()
runOnline ai url = runSession chromeConfig . finallyClose $ do
    openPage url
    ignoreReturn $ executeJS [] extractGameTrackFrameJS

    flip execStateT (M.empty, 0) . sequence . repeat $ do
        liftIO $ putStrLn "Waiting for game to start..."
        lift waitForGameStart
        liftIO $ putStrLn "Game starting!"
        (hist, pcs) <- get
        (lines, ct) <- lift (mainLoop ai)
        if not (null lines) then do
            let hist' = updateHist hist lines
            put (hist', pcs + ct)
            liftIO . print $ hist'
            liftIO . print $ pcs + ct
        else pure ()
        liftIO $ putStrLn "Game complete!"
    pure ()

-- This function injects some code into the render loop which lets us keep track
-- of frames. It also puts the Game instance in a global variable so we can directly
-- read the state.
extractGameTrackFrameJS :: Text
extractGameTrackFrameJS = " window.fcount = 0; \
                          \ window.game = null; \
                          \ var rd = Game.prototype.redraw; \
                          \ Game.prototype.redraw = function() { \
                          \     rd.apply(this,arguments); \
                          \     window.game = this; \
                          \     window.fcount += 1; \
                          \ };"

-----------------------
-----------------------
-- | Simulate Code | --
-----------------------
-----------------------

runSimulation :: AIState -> Bool -> IO ()
runSimulation ai v = flip evalStateT ai . go 0 . startingState =<< getStdGen
    where go :: Int -> SimulatorState -> StateT AIState IO ()
          go n st = do
              when v . liftIO . (>> putStrLn "") . printBoard . addActiveBlock (board . gs $ st) . active . gs $ st
              acts <- runAI 10 (gs st)
              let acts' :: [Maybe SimulatorState -> StateT AIState IO (Maybe SimulatorState)]
                  acts' = fmap (\(a,_) -> fmap (fmap fst . join) . sequence . fmap (advance n a)) acts
              st' <- foldl (>=>) (pure . id) acts' (Just st)
              case st' of
                Just s -> go (n + 1) s
                Nothing -> pure ()

-----------------------
-----------------------
-- | Train GA Code | --
-----------------------
-----------------------

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
    nxt <- advance (stp ts) act (ss ts)
    if isNothing nxt || (act /= HardDrop && (kp ts) == 10)
       then nextEp ts{rollout = (-1,grad):(rollout ts)}
       else let (ss', atk) = fromJust nxt
                stp' = 1 + (stp ts)
                hd  = act == HardDrop
                kp' = if hd then 0 else 1 + (kp ts)
                rwd = realToFrac $ 2 * atk + (if hd then 1 else 0)
                rl' = (rwd, grad):(rollout ts)
             in pure ts{ss=ss', as=as', stp=stp', kp=kp', rollout=rl'}


runTraining :: Adam (Gradients NL) -> Bool -> IO ()
runTraining a v = go =<< TState <$> fmap startingState getStdGen <*> defaultState <*> pure 0 <*> pure 0 <*> pure [] <*>  pure a <*> pure 0 <*> pure 0
    where go :: TState -> IO ()
          go = step v >=> go
