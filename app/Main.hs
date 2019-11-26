{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Random.Strict
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Control.Parallel.Strategies (parMap, rpar)
import Data.Aeson
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time.Clock
import Moo.GeneticAlgorithm.Continuous
import qualified Moo.GeneticAlgorithm.Continuous as MOO
import Numeric.LinearAlgebra.Static (matrix)
import System.Environment (getArgs)
import System.Random
import Test.WebDriver
import Test.WebDriver.Commands.Wait
import Test.WebDriver.JSON (ignoreReturn)
import Text.Printf

import AI
import CLI
import Parse
import Simulator
import Tetris

main :: IO ()
main = do
    cmd <- processCLI
    case cmd of
      Run a u -> runOnline a u
      Simulate a v -> runSimulation a v
      Train i -> runTraining i

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

mainLoop :: Maybe AIState -> WD (Lines, Int)
mainLoop ai = fmap fst . evalRandTIO . runStateT (go (0, (M.empty, 0))) =<< liftIO (maybe defaultState pure ai)
    where go :: (Int, (Lines, Int)) -> StateT AIState (RandT StdGen WD) (Lines, Int)
          go = guardInGame $ \(curr, (h, pct)) -> do
              (curr', state, inc) <- lift . lift . nextState $ curr
              let h' = foldr id h . fmap (\[size, id] -> M.insertWith (flip const) id size) $ inc
              -- liftIO . printBoard . addActiveBlock (board state) . active $ state 

              keys <- runAI state
              -- liftIO . putStrLn $ "Keys: " <> show keys

              body <- lift . lift . findElem $ ByTag "body"
              lift . lift . sendKeys (mconcat . fmap actionToText $ keys) $ body

              go (curr', (h', pct + 1))
          guardInGame :: ((Int, (Lines, Int)) -> StateT AIState (RandT StdGen WD) (Lines, Int)) -> (Int, (Lines, Int)) -> StateT AIState (RandT StdGen WD) (Lines, Int)
          guardInGame act p = do
              running <- lift (lift inGame)
              if running
                 then act p
                 else pure . snd $ p

updateHist :: Histogram -> Lines -> Histogram
updateHist = M.foldr (\v -> M.insertWith (+) v 1)

runOnline :: Maybe AIState -> String -> IO ()
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

runSimulation :: Maybe AIState -> Bool -> IO ()
runSimulation ai v = do
    let sim = if v then simulateAILog else simulateAI
    g <- getStdGen
    (pcs, atks) <- sim 500 g =<< maybe defaultState pure ai
    putStrLn $ "Placed: " ++ (show pcs) ++ "\nLines Sent: " ++ (show atks)
    pure ()

-----------------------
-----------------------
-- | Train GA Code | --
-----------------------
-----------------------

popsize = 53
elitesize = 3
gens = 1000

evaluate :: RandomGen g => g -> Genome Double -> Objective
evaluate g pop = maximum . flip evalState g . sequence . replicate 3 . state $ simulate
    where simulate :: RandomGen g => g -> (Objective, g)
          simulate g = (\((a,b), g) -> (fromInteger . toInteger $ a + b, g)) . runIdentity . simulateAI 500 g . listToAI $ pop

evaluate' :: RandomGen g => g -> [Genome Double] -> [Objective]
evaluate' g = parMap rpar (evaluate g)

selection = stochasticUniversalSampling (popsize - elitesize)
crossover = twoPointCrossover 0.3
mutation = gaussianMutate 0.1 5.0
initialize = getRandomGenomes popsize (replicate (7 * 14 + 8) (-10,10))
step :: StepGA MOO.Rand Double
step x y = do
    g <- liftRand (\x -> (x,x))
    nextGeneration Maximizing (evaluate' g) selection elitesize crossover mutation x y
stop = IfObjective ((> 700) . maximum)

stepFunc :: Int -> Population Double -> IO ()
stepFunc g pop = do
    print =<< getCurrentTime
    printf "Gen: %d \n Pop: %s \n" g (show . fmap snd $ pop)
    writeFile ("population_" ++ show g) (show pop)
    printf "Saved pop!\n"

runTraining :: Either Bool (Population Double) -> IO ()
runTraining i = do
    init <- case i of
              Left False -> pure initialize
              Left True  -> fmap (pure . fmap fst) (readLn :: IO (Population Double))
              Right pop  -> pure . pure . fmap fst $ pop
    population <- runIO init (loopIO [DoEvery 1 stepFunc] stop step)
    print . head . bestFirst Maximizing $ population
