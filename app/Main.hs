{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Test.WebDriver
import Test.WebDriver.Commands.Wait

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

waitForGameStart :: WD ()
waitForGameStart = waitUntil' 10000 10 $ do
    -- Jstris puts several overlays over the game with the class "gCapt"
    -- Once they all disappear, the game has started
    -- So, we simply wait for all of the elements with the class "gCapt" to have "display: none" set.
    -- However, if not all of the overlays have been created, all of the current overlays may be invisible, but the game has not yet started
    -- So, we also need to wait for there to be exactly 3 overlays
    capts <- findElems ( ByCSS "#stage .gCapt" )
    expect $ length capts == 3
    disp <- sequence .  fmap (\e -> cssProp e "display") $ capts
    expect . all (maybe True (== "none")) $ disp

main :: IO ()
main = runSession chromeConfig . finallyClose $ do
    -- We're starting by targeting the Cheese Race.
    openPage "https://jstris.jezevec10.com/?play=3&mode=1"

    liftIO $ putStrLn "Waiting for game to start..."
    waitForGameStart
    liftIO $ putStrLn "Game starting!"

    -- PoC: press space a bunch once the game starts
    body <- findElem ( ByCSS "body")
    sendKeys "        " body 

    -- Delay 10 seconds so we can see what happened
    liftIO $ threadDelay 10000000
