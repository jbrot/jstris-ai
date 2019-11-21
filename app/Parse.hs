{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Control.Monad
import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Test.WebDriver.Common.Keys as K

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

actionToText :: Action -> Text
actionToText MoveLeft    = K.arrowLeft
actionToText MoveRight   = K.arrowRight
actionToText SoftDrop    = K.arrowDown
actionToText HardDrop    = " "
actionToText RotateLeft  = "z"
actionToText RotateRight = K.arrowUp
actionToText Hold        = "c"
