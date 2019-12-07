module Tetris.Action (Action (..), dropBlock, moveBlock, moveBlock') where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe

import Tetris.Block
import Tetris.Board

data Action = MoveLeft | MoveRight | SoftDrop | HardDrop | RotateLeft | RotateRight | Hold
    deriving (Eq, Show)

-- Given an ActiveBlock, returns a new ActiveBlock in the position the current block will drop to.
-- Will only return Nothing if the current position is invalid.
dropPosition :: Board -> ActiveBlock -> Maybe ActiveBlock
dropPosition board = fmap (\a@ActiveBlock{ pos = (r,c) } -> fromMaybe a . dropPosition board $ a{ pos = (r + 1, c) }) . validateAB board

dropBlock :: Board -> ActiveBlock -> Board
dropBlock board ab = addActiveBlock board . fromMaybe ab . dropPosition board $ ab

-- True: rotates the block right, False: rotates left.
-- This is actually reasonably complicated as it will resolve kicks.
-- Returns Nothing if no rotation position is valid.
rotateBlock :: Board -> Bool -> ActiveBlock -> Maybe ActiveBlock
rotateBlock board dir (ActiveBlock k (r,c) rot) = listToMaybe . catMaybes . fmap (validateAB board) $ candidates
    where nrot = if dir then (rot + 1) `mod` 4 
                        else (rot + 3) `mod` 4
          kicks = if k == I then kickMap M.! (I, rot, dir)
                            else kickMap M.! (J, rot, dir)
          candidates = fmap (\(ro,co) -> ActiveBlock k (r + ro, c + co) nrot) kicks

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

-- True: right; False: left
kickMap :: Map (Block, Rot, Bool) [Pos]
kickMap = M.fromList [ ((I, 0, True),  [ (0,0), (-2,0), ( 1,0), (-2,-1), ( 1, 2) ])
                     , ((I, 0, False), [ (0,0), (-1,0), ( 2,0), (-1, 2), ( 2,-1) ])
                     , ((I, 1, True),  [ (0,0), (-1,0), ( 2,0), (-1, 2), ( 2,-1) ])
                     , ((I, 1, False), [ (0,0), ( 2,0), (-1,0), ( 2, 1), (-1,-2) ])
                     , ((I, 2, True),  [ (0,0), ( 2,0), (-1,0), ( 2, 1), (-1,-2) ])
                     , ((I, 2, False), [ (0,0), ( 1,0), (-2,0), ( 1,-2), (-2, 1) ])
                     , ((I, 3, True),  [ (0,0), ( 1,0), (-2,0), ( 1,-2), (-2, 1) ])
                     , ((I, 3, False), [ (0,0), (-2,0), ( 1,0), (-2,-1), ( 1, 2) ])

                     , ((J, 0, True),  [ (0,0), (-1,0), (-1, 1), (0,-2), (-1,-2) ])
                     , ((J, 0, False), [ (0,0), ( 1,0), ( 1, 1), (0,-2), ( 1,-2) ])
                     , ((J, 1, True),  [ (0,0), ( 1,0), ( 1,-1), (0, 2), ( 1, 2) ])
                     , ((J, 1, False), [ (0,0), ( 1,0), ( 1,-1), (0, 2), ( 1, 2) ])
                     , ((J, 2, True),  [ (0,0), ( 1,0), ( 1, 1), (0,-2), ( 1,-2) ])
                     , ((J, 2, False), [ (0,0), (-1,0), (-1, 1), (0,-2), (-1,-2) ])
                     , ((J, 3, True),  [ (0,0), (-1,0), (-1,-1), (0, 2), (-1, 2) ])
                     , ((J, 3, False), [ (0,0), (-1,0), (-1,-1), (0, 2), (-1, 2) ])
                     ]
