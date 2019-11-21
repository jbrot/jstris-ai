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

import qualified Parse as P
import AI
import Tetris

idsToBlock :: Map Int Block
idsToBlock = M.fromList [ (0, I)
                        , (1, O)
                        , (2, T)
                        , (3, L)
                        , (4, J)
                        , (5, S)
                        , (6, Z)
                        ]

colorsToSquare :: Map Int Square
colorsToSquare = M.fromList [ (0, Empty)
                            , (1, Remnant Z)
                            , (2, Remnant L)
                            , (3, Remnant O)
                            , (4, Remnant S)
                            , (5, Remnant I)
                            , (6, Remnant J)
                            , (7, Remnant T)
                            , (8, Garbage)
                            , (9, Garbage)
                            ]

instance FromJSON ActiveBlock where
    parseJSON (Object v) = ActiveBlock <$> fmap (idsToBlock M.!) (v .: "id")
                                       <*> (parsePos =<< v.: "pos")
                                       <*> v .: "rot"
        where parsePos (Object v) = (,) <$> v .: "y" <*> v .: "x"
              parsePos _ = mzero
    parseJSON _ = mzero

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

inGame :: WD Bool
inGame = not <$> executeJS [] "return window.game == null || window.game.gameEnded"

waitForGameStart :: WD ()
waitForGameStart = waitUntil' 10000 600 $ do
    expect =<< inGame

-- Given a frame index, waits for a higher frame index and then retrieves the associated frame.
getNextFrame :: Int -> WD (Int, [[P.Square]], GameState)
getNextFrame curr = waitUntil' 10000 10 $ do
    count <- executeJS [] "return window.pcount;"
    expect $ count > curr
    frame <- P.parseFrame <$> executeJS [] "return window.pixels;"

    matrix <- executeJS [] "return window.game.matrix"
    act    <- executeJS [] "return window.game.activeBlock"
    hld    <- executeJS [] "return window.game.blockInHold"
    q      <- executeJS [] "return window.game.queue"
    let brd = fmap (colorsToSquare M.!) . mconcat $ matrix
        gs = GameState brd act (kind <$> hld) (kind <$> q)

    return (count, frame, gs)

mainLoop :: WD ()
mainLoop = runStateT (go 0) defaultState >> return ()
    where go :: Int -> StateT AIState WD Int
          go curr = guardInGame $ do
              notDone <- lift inGame

              (curr', frame, state) <- lift . getNextFrame $ curr
              liftIO . putStrLn . show $ curr'
              liftIO . P.printFrame $ frame
              liftIO $ putStrLn ""
              liftIO . printBoard . renderBlock (active state) . board $ state 

              keys <- fmap (mconcat . fmap P.actionToText) . runAI . P.filterInactive $ frame
              liftIO . T.putStrLn $ "Keys: " <> keys

              body <- findElem ( ByTag "body" )
              sendKeys keys body

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

    game <- findElem ( ById "myCanvas" )
    (width, height) <- elemSize game
    ignoreReturn $ executeJS [ JSArg game, JSArg width, JSArg height ] extractColorsJS

    liftIO $ putStrLn "Waiting for game to start..."
    waitForGameStart
    liftIO $ putStrLn "Game starting!"

    sequence . repeat $ mainLoop
    pure ()

-- This is a JS function to inject, which will get the color at the center of each square in the canvas
extractColorsJS :: Text
extractColorsJS = " var ctx = arguments[0].getContext('webgl'); \
                  \ var width = arguments[1]; \
                  \ var height = arguments[2]; \
                  \ window.pbuffer = new Uint8Array(4 * width * height); \
                  \ window.pcount = 0; \
                  \ window.game = null; \
                  \ var cc = Game.prototype.redraw; \
                  \ Game.prototype.redraw = function() { \
                  \     cc.apply(this,arguments); \
                  \     window.game = this; \
                  \     ctx.readPixels(0, 0, width, height, ctx.RGBA, ctx.UNSIGNED_BYTE, window.pbuffer); \
                  \     var iter = function*() { \
                  \         var NUM_COLS = 10; \
                  \         var NUM_ROWS = 20; \
                  \         var cw = width / NUM_COLS; \
                  \         var ch = height / NUM_ROWS; \
                  \         var r,c,i; \
                  \         for (r = NUM_ROWS - 1; r >= 0; r--) { \
                  \             for (c = 0; c < NUM_COLS; c++) { \
                  \                 i = (r + 0.5) * ch * width + (c + 0.5) * cw; \
                  \                 i = Math.floor(i) * 4; \
                  \                 yield* window.pbuffer.subarray(i, i + 4); \
                  \             } \                  
                  \         } \
                  \     }(); \
                  \     window.pixels = new Uint8Array(iter); \
                  \     window.pcount += 1; \
                  \ };"
