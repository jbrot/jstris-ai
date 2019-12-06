{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Control.Monad
import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Test.WebDriver.Common.Keys as K

import Tetris
import Tetris.Block
import Tetris.Board

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
                            , (1, Garbage)
                            , (2, Garbage)
                            , (3, Garbage)
                            , (4, Garbage)
                            , (5, Garbage)
                            , (6, Garbage)
                            , (7, Garbage)
                            , (8, Garbage)
                            , (9, HurryUp)
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
