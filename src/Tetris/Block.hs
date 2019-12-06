{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Tetris.Block where

import Data.Finitary
import Data.Finitary.Finiteness
import GHC.Generics

data Block = I | J | L | O | S | T | Z
    deriving (Eq, Show, Ord, Enum, Generic, Finitary)

type Row = Int
type Col = Int
type Pos = (Row, Col)

type Rot = Int

data ActiveBlock = ActiveBlock { kind :: Block
                               , pos :: Pos
                               , rot :: Rot
                               }

startingPosition :: Block -> ActiveBlock
startingPosition b = ActiveBlock b (height b, 3) 0
  where height I = -1
        height _ = -2
