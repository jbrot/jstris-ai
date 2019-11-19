{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import Text.Printf
import Test.WebDriver
import Test.WebDriver.Commands.Wait
import Test.WebDriver.JSON (ignoreReturn)

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

-- Outputs the pixels extracted to the terminal
renderFrame :: [Int] -> IO ()
renderFrame cs = go 0 0 cs
    where go :: Int -> Int -> [Int] -> IO ()
          go  20  _ _ = putStrLn "\x1b[0m"
          go row 10 colors = do
                printf "\x1b[0m\n"
                go (row + 1) 0 colors
          go row col (r:g:b:a:colors) = do
                printf "\x1b[48;2;%d;%d;%dm " r g b
                go row (col + 1) colors
          go _ _ _ = putStrLn "\x1b[0m"
    

-- Extracts and renders pixels.
getFrame :: Int -> WD Int
getFrame curr = waitUntil' 10000 10 $ do
    count <- executeJS [] "return window.pcount;"
    expect $ count > curr
    liftIO $ print count
    display <- executeJS [] "return window.pixels;"
    liftIO $ renderFrame display
    return count 

iterateM_ :: Monad m => (a -> m a) -> a -> m a
iterateM_ f a = iterateM_ f =<< f a

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

    iterateM_ getFrame 0

    -- PoC: press space a bunch once the game starts
    body <- findElem ( ByCSS "body")
    sendKeys "        " body 

    -- Delay 10 seconds so we can see what happened
    liftIO $ threadDelay (10 * 1000000)

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
