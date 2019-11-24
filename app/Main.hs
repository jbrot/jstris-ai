{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import System.Random
import Test.WebDriver
import Test.WebDriver.Commands.Wait
import Test.WebDriver.JSON (ignoreReturn)

import AI
import Parse
import Simulator
import Tetris

-- An example GameState for debugging
exMatrix = [ [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
           , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
           , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
           , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
           , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
           , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
           , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
           , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
           , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
           , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
           , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
           , [ 8, 8, 8, 8, 8, 0, 8, 8, 8, 8 ]
           , [ 8, 8, 8, 8, 8, 8, 8, 0, 8, 8 ]
           , [ 8, 8, 8, 8, 8, 8, 0, 8, 8, 8 ]
           , [ 8, 8, 8, 8, 8, 0, 8, 8, 8, 8 ]
           , [ 8, 8, 8, 8, 8, 8, 8, 8, 0, 8 ]
           , [ 8, 8, 8, 8, 8, 8, 8, 0, 8, 8 ]
           , [ 8, 8, 8, 8, 8, 8, 0, 8, 8, 8 ]
           , [ 8, 8, 8, 8, 8, 8, 8, 8, 0, 8 ]
           , [ 8, 8, 8, 0, 8, 8, 8, 8, 8, 8 ]
           ]
exBrd = V.fromList . fmap (colorsToSquare M.!) . mconcat $ exMatrix
exAct = ActiveBlock Z (-1, 3) 0
exState = GameState exBrd exAct Nothing [] 0

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

    matrix <- executeJS [] "return window.game.matrix"
    act    <- executeJS [] "return window.game.activeBlock"
    hld    <- executeJS [] "return window.game.blockInHold"
    q      <- executeJS [] "return window.game.queue"
    inc    <- executeJS [] "return window.game.incomingGarbage"
    let brd = fmap (colorsToSquare M.!) . mconcat $ matrix
        garbage = sum . fmap head $ inc
        gs = GameState brd act (kind <$> hld) (kind <$> q) garbage
    pure (count, gs, inc)

type Lines = Map Int Int
type Histogram = Map Int Int

mainLoop :: WD (Lines, Int)
mainLoop = fst <$> runStateT (go (0, (M.empty, 0))) defaultState
    where go :: (Int, (Lines, Int)) -> StateT AIState WD (Lines, Int)
          go = guardInGame $ \(curr, (h, pct)) -> do
              (curr', state, inc) <- lift . nextState $ curr
              let h' = foldr id h . fmap (\[size, id] -> M.insertWith (flip const) id size) $ inc
              -- liftIO . printBoard . addActiveBlock (board state) . active $ state 

              keys <- runAI state
              -- liftIO . putStrLn $ "Keys: " <> show keys

              body <- findElem ( ByTag "body" )
              sendKeys (mconcat . fmap actionToText $ keys) body

              go (curr', (h', pct + 1))
          guardInGame act p = do
              running <- lift inGame
              if running
                 then act p
                 else pure . snd $ p

updateHist :: Histogram -> Lines -> Histogram
updateHist = M.foldr (\v -> M.insertWith (+) v 1)

main :: IO ()
-- main = runSession chromeConfig . finallyClose $ do
--     openPage "https://jstris.jezevec10.com/"
--     ignoreReturn $ executeJS [] extractGameTrackFrameJS
-- 
--     flip execStateT (M.empty, 0) . sequence . repeat $ do
--         liftIO $ putStrLn "Waiting for game to start..."
--         lift waitForGameStart
--         liftIO $ putStrLn "Game starting!"
--         (hist, pcs) <- get
--         (lines, ct) <- lift mainLoop
--         if not (null lines) then do
--             let hist' = updateHist hist lines
--             put (hist', pcs + ct)
--             liftIO . print $ hist'
--             liftIO . print $ pcs + ct
--         else pure ()
--         liftIO $ putStrLn "Game complete!"
--     pure ()

-- An alternate main which runs the AI on a simulated game.
main =  do
    g <- getStdGen
    (pcs, atks) <- simulateAI g 1000 defaultState
    putStrLn $ "Placed: " ++ (show pcs) ++ "\nLines Sent: " ++ (show atks)
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
