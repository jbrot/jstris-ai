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
import Test.WebDriver
import Test.WebDriver.Commands.Wait
import Test.WebDriver.JSON (ignoreReturn)

import AI
import Parse
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
exState = GameState exBrd exAct Nothing []

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

inGame :: WD Bool
inGame = not <$> executeJS [] "return window.game == null || window.game.gameEnded"

waitForGameStart :: WD ()
waitForGameStart = waitUntil' 10000 600 $ do
    expect =<< inGame

-- Given a frame index, waits for a higher frame index and then retrieves the associated frame.
nextState :: Int -> WD (Int, GameState)
nextState curr = waitUntil' 10000 10 $ do
    count <- executeJS [] "return window.fcount;"
    expect $ count > curr

    matrix <- executeJS [] "return window.game.matrix"
    act    <- executeJS [] "return window.game.activeBlock"
    hld    <- executeJS [] "return window.game.blockInHold"
    q      <- executeJS [] "return window.game.queue"
    let brd = fmap (colorsToSquare M.!) . mconcat $ matrix
        gs = GameState brd act (kind <$> hld) (kind <$> q)
    pure (count, gs)

mainLoop :: WD ()
mainLoop = runStateT (go 0) defaultState >> return ()
    where go :: Int -> StateT AIState WD Int
          go curr = guardInGame $ do
              (curr', state) <- lift . nextState $ curr
              liftIO . printBoard . addActiveBlock (board state) . active $ state 

              keys <- runAI state
              liftIO . putStrLn $ "Keys: " <> show keys

              body <- findElem ( ByTag "body" )
              sendKeys (mconcat . fmap actionToText $ keys) body

              go curr'
          guardInGame act = do
              running <- lift inGame
              if running
                 then act
                 else pure 0

main :: IO ()
main = runSession chromeConfig . finallyClose $ do
    -- We're starting by targeting the Cheese Race.
    openPage "https://jstris.jezevec10.com/?play=3&mode=1"
    ignoreReturn $ executeJS [] extractGameTrackFrameJS

    liftIO $ putStrLn "Waiting for game to start..."
    waitForGameStart
    liftIO $ putStrLn "Game starting!"

    sequence . repeat $ mainLoop
    pure ()

-- An alternate main which runs the AI on the example state
-- main = print =<< runStateT (runAI exState) defaultState

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
