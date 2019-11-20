{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text.IO as T
import Test.WebDriver
import Test.WebDriver.Commands.Wait
import Test.WebDriver.JSON (ignoreReturn)

import Parse
import AI

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

waitForGameStart :: WD ()
waitForGameStart = waitUntil' 10000 600 $ do
    -- Jstris puts several overlays over the game with the class "gCapt"
    -- Once they all disappear, the game has started
    -- So, we simply wait for all of the elements with the class "gCapt" to have "display: none" set.
    -- However, if not all of the overlays have been created, all of the current overlays may be invisible, but the game has not yet started
    -- So, we also need to wait for there to be exactly 3 overlays
    capts <- findElems ( ByCSS "#stage .gCapt" )
    expect $ length capts == 3
    disp <- sequence .  fmap (\e -> cssProp e "display") $ capts
    expect . all (maybe True (== "none")) $ disp
    

-- Given a frame index, waits for a higher frame index and then retrieves the associated frame.
getNextFrame :: Int -> WD (Int, [[Square]])
getNextFrame curr = waitUntil' 10000 10 $ do
    count <- executeJS [] "return window.pcount;"
    expect $ count > curr
    display <- executeJS [] "return window.pixels;"
    return (count, parseFrame display)

mainLoop :: WD ()
mainLoop = runStateT (go 0) defaultState >> return ()
    where go :: Int -> StateT AIState WD Int
          go curr = do
              (curr', frame) <- lift . getNextFrame $ curr
              liftIO . putStrLn . show $ curr'
              liftIO . printFrame . filterInactive $ frame

              keys <- fmap (mconcat . fmap actionToText) . runAI . filterInactive $ frame
              liftIO . T.putStrLn $ "Keys: " <> keys

              body <- findElem ( ByTag "body" )
              sendKeys keys body
              
              go curr'

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

    mainLoop

-- This is a JS function to inject, which will get the color at the center of each square in the canvas
extractColorsJS :: Text
extractColorsJS = " var ctx = arguments[0].getContext('webgl'); \
                  \ var width = arguments[1]; \
                  \ var height = arguments[2]; \
                  \ window.pbuffer = new Uint8Array(4 * width * height); \
                  \ var cc = ctx.drawArrays; \
                  \ ctx.drawArrays = function() { \
                  \     cc.apply(this,arguments); \
                  \     ctx.readPixels(0, 0, width, height, ctx.RGBA, ctx.UNSIGNED_BYTE, window.pbuffer); \
                  \ }; \
                  \ window.pcount = 0; \
                  \ var cc2 = ctx.clear; \
                  \ ctx.clear = function() { \
                  \     cc2.apply(this, arguments); \
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
                  \ }; "
