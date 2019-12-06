{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, DerivingVia, PatternSynonyms #-}
module Tetris -- ( Block (I, J, L, O, S, T, Z)
              -- , Row, Col, Pos, Rot
              -- , ActiveBlock (ActiveBlock), kind, pos, rot, getCoords, startingPosition
              -- , Square (Empty, Garbage, Remnant, HurryUp)
              -- , Board, boardIndex, getSquare, isEmpty, canAddActiveBlock, validateAB, addActiveBlock, dropPosition, dropBlock, rotateBlock, complete, clearLines, addGarbageLines, attackLines, printBoard
              -- , Action (MoveLeft, MoveRight, SoftDrop, HardDrop, RotateLeft, RotateRight, Hold), moveBlock, moveBlock'
              -- , GameState (GameState), board, active, held, queue, garbage, moveActive, moveActive', addActive, clearLines', addGarbage, reduceGarbage
              -- , pack, unpack
              -- ) where
   where

import Control.Monad.Random
import Data.Bits
import Data.Finite
import Data.Finitary
import Data.Finitary.Finiteness
import Data.Finitary.PackInto (PackInto)
import Data.Functor.Identity
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Vector.Unboxed.Sized (Vector)
import qualified Data.Vector.Unboxed.Sized as U
import qualified Data.Vector.Sized as V
import Data.Word
import GHC.Generics
import Text.Printf

import Tetris.Block
import Tetris.Board


data Action = MoveLeft | MoveRight | SoftDrop | HardDrop | RotateLeft | RotateRight | Hold
    deriving (Eq, Show)

-- Applies an action to a block.
-- Does nothing if the specified Action is Hold.
-- Returns Nothing if the Action fails.
moveBlock :: Board -> Action -> ActiveBlock -> Maybe ActiveBlock
moveBlock _ Hold a = Just a
moveBlock b MoveLeft  a@ActiveBlock{pos = (r,c)} = validateAB b a{pos = (r, c - 1) }
moveBlock b MoveRight a@ActiveBlock{pos = (r,c)} = validateAB b a{pos = (r, c + 1) }
moveBlock b SoftDrop  a@ActiveBlock{pos = (r,c)} = validateAB b a{pos = (r + 1, c) }
moveBlock b HardDrop    a = dropPosition b a
moveBlock b RotateLeft  a = rotateBlock b False a
moveBlock b RotateRight a = rotateBlock b True a

-- Same as moveBlock, but returns the given ActiveBlock if the Action fails.
moveBlock' :: Board -> Action -> ActiveBlock -> ActiveBlock
moveBlock' b a ab = fromMaybe ab (moveBlock b a ab)

data GameState = GameState { board :: Board
                           , active :: ActiveBlock
                           , held :: Maybe Block
                           , queue :: [Block]
                           , garbage :: [Int]
                           }

moveActive :: Action -> GameState -> Maybe GameState
moveActive act gs = fmap (\a -> gs{active = a}) $ moveBlock (board gs) act (active gs)

moveActive' :: Action -> GameState -> GameState
moveActive' a s = fromMaybe s (moveActive a s)

addActive :: GameState -> GameState
addActive g = g{board = addActiveBlock (board g) (active g)}

clearLines' :: GameState -> (Int, GameState)
clearLines' gs = fmap (\b -> gs{board = b}) . clearLines . board $ gs

addGarbage :: MonadRandom m => GameState -> m GameState
addGarbage g = fmap (\b -> g{board = b, garbage = []}) $ foldl update (pure $ board g) (garbage g)
  where update :: MonadRandom m => m Board -> Int -> m Board
        update b' ct = do
            cl <- getRandomR (0,9)
            pure . addGarbageLines ct cl =<< b'

reduceGarbage :: Int -> GameState -> GameState
reduceGarbage _ g@GameState{garbage = [] } = g
reduceGarbage c g@GameState{garbage = ct:gbs}
  | c >= ct  = reduceGarbage (c - ct) g{garbage = gbs}
  | otherwise = g{garbage = (ct - c):gbs}
