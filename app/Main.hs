{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.WebDriver

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

main :: IO ()
main = runSession chromeConfig . finallyClose $ do
    openPage "https://google.com"
    searchInput <- findElem ( ByCSS "input[type='text']")
    sendKeys "Hello, World!" searchInput
    submit searchInput
    closeSession
