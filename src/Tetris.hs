module Tetris ( Block (I, J, L, O, S, T, Z)
              , Row, Col, Pos, Rot
              , ActiveBlock (ActiveBlock), kind, pos, rot, getCoords, startingPosition
              , Square (Empty, Garbage, Remnant, HurryUp)
              , Board, boardIndex, getSquare, isEmpty, canAddActiveBlock, addActiveBlock, dropPosition, dropBlock, rotateBlock, complete, clearLines, printBoard
              , GameState (GameState), board, active, held, queue, garbage
              , Action (MoveLeft, MoveRight, SoftDrop, HardDrop, RotateLeft, RotateRight, Hold), moveBlock
              ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.Printf

data Block = I | J | L | O | S | T | Z
  deriving (Eq, Ord, Show)

type Row = Int
type Col = Int
type Pos = (Row, Col)

type Rot = Int

data ActiveBlock = ActiveBlock { kind :: Block
                               , pos :: Pos
                               , rot :: Rot
                               }

getCoords :: ActiveBlock -> [Pos]
getCoords b = fmap (\(r,c) -> (r + (fst . pos $ b), c + (snd . pos $ b))) $ rotMap M.! (kind b, rot b)

startingPosition :: Block -> ActiveBlock
startingPosition b = ActiveBlock b (height b, 3) 0
  where height I = -1
        height _ = -2

data Square = Empty | Garbage | Remnant Block | HurryUp
    deriving (Eq, Show)

type Board = Vector Square

boardIndex :: Pos -> Int
boardIndex (r,c) = 10 * r + c

getSquare :: Pos -> Board -> Square
getSquare p b = b V.! (boardIndex p)

isEmpty :: Board -> Pos -> Bool
isEmpty b (r,c) 
  | c <  0  = False
  | c >= 10 = False
  | r <  0  = True
  | r >= 20 = False
  | otherwise = b V.! boardIndex (r,c) == Empty

-- Are all the spaces occupied by the ActiveBlock empty?
canAddActiveBlock :: Board -> ActiveBlock -> Bool 
canAddActiveBlock board = and . fmap (isEmpty board) . getCoords

-- Replaces the squares in the board the ActiveBlock occupies with the appropriate remnants.
-- Does not check if spaces are overwritten.
addActiveBlock :: Board -> ActiveBlock -> Board
addActiveBlock board block = board V.// (filter (\(i,_) -> (i >= 0) && (i < 200)) updates)
    where updates = fmap (\p -> (boardIndex p, Remnant (kind block))) . getCoords $ block

-- Given an ActiveBlock, returns a new ActiveBlock in the position the current block will drop to.
dropPosition :: Board -> ActiveBlock -> ActiveBlock
dropPosition board block@ActiveBlock{ pos = p } = block { pos = maybe p id $ iterate p }
  where iterate :: Pos -> Maybe Pos
        iterate (r,c) = if canAddActiveBlock board (block { pos = (r,c) })
                           then maybe (Just (r,c)) Just $ iterate (r + 1,c) 
                           else Nothing

dropBlock :: Board -> ActiveBlock -> Board
dropBlock board = addActiveBlock board . dropPosition board

-- True: rotates the block right, False: rotates left.
-- This is actually reasonably complicated as it will resolve kicks.
rotateBlock :: Board -> Bool -> ActiveBlock -> ActiveBlock
rotateBlock board dir def@(ActiveBlock k (r,c) rot) = maybe def id . listToMaybe . filter (canAddActiveBlock board) $ candidates
    where nrot = if dir then (rot + 1) `mod` 4 
                        else (rot + 3) `mod` 4
          kicks = if k == I then kickMap M.! (I, rot, dir)
                            else kickMap M.! (J, rot, dir)
          candidates = fmap (\(ro,co) -> ActiveBlock k (r + ro, c + co) nrot) kicks

complete :: Board -> Row -> Bool
complete b r = null . V.filter (\s -> s == Empty || s == HurryUp) .  V.slice (10 * r) 10 $ b

clearLines :: Board -> (Int, Board)
clearLines board = foldr remove (0, board) . filter (complete board) . reverse $ [0..19] 
    where remove :: Row -> (Int, Board) -> (Int, Board)
          remove r (c, b) = (c + 1, V.modify (\v -> do
              MV.move (MV.slice 10 (10 * r) v) (MV.slice 0 (10 * r) v) 
              MV.set (MV.slice 0 10 v) Empty) b)

data GameState = GameState { board :: Board
                           , active :: ActiveBlock
                           , held :: Maybe Block
                           , queue :: [Block]
                           , garbage :: Int
                           }

data Action = MoveLeft | MoveRight | SoftDrop | HardDrop | RotateLeft | RotateRight | Hold
  deriving Show

-- Applies an action to a block.
-- Does nothing if the specified Action is Hold.
moveBlock :: Board -> Action -> ActiveBlock -> ActiveBlock
moveBlock _ Hold a = a
moveBlock b MoveLeft  a@ActiveBlock{pos = (r,c)} = if canAddActiveBlock b a' then a'
                                                                             else a
    where a' = a{pos = (r,c - 1)}
moveBlock b MoveRight a@ActiveBlock{pos = (r,c)} = if canAddActiveBlock b a' then a'
                                                                             else a
    where a' = a{pos = (r,c + 1)}
moveBlock b SoftDrop  a@ActiveBlock{pos = (r,c)} = if canAddActiveBlock b a' then a'
                                                                             else a
    where a' = a{pos = (r + 1,c)}
moveBlock b HardDrop    a = dropPosition b a
moveBlock b RotateLeft  a = rotateBlock b False a
moveBlock b RotateRight a = rotateBlock b True a

printBoard :: Board -> IO ()
printBoard board = (>> return ()) . sequence . fmap (printRow board) $ [0..19]
  where printSquare :: Square -> IO ()
        printSquare s = let (r,g,b) = sqColor s in printf "\x1b[48;2;%d;%d;%dm%c" r g b (sqChar s)
        printRow :: Board -> Row -> IO ()
        printRow b r = (>> printf "\x1b[0m\n") . sequence . fmap (printSquare . (\c -> getSquare (r, c) b)) $ [0..9]
        sqColor :: Square -> (Int, Int, Int)
        sqColor Empty = (0,0,0)
        sqColor Garbage = (115,115,115)
        sqColor HurryUp = (106,106,106)
        sqColor (Remnant b) = colorMap M.! b
        sqChar :: Square -> Char
        sqChar Empty   = ' '
        sqChar Garbage = 'X'
        sqChar HurryUp = 'X'
        sqChar (Remnant b) = head . show $ b

colorMap :: Map Block (Int, Int, Int)
colorMap = M.fromList [ (I, ( 15,155,215))
                      , (J, ( 33, 65,198))
                      , (L, (227, 91,  2))
                      , (O, (227,159,  2))
                      , (S, ( 89,177,  1))
                      , (T, (175, 41,138))
                      , (Z, (215, 15, 55))
                      ]

rotMap :: Map (Block, Rot) [Pos]
rotMap = M.fromList [ ((I, 0), [ (1,0), (1,1), (1,2), (1,3) ])
                    , ((I, 1), [ (0,2), (1,2), (2,2), (3,2) ])
                    , ((I, 2), [ (2,0), (2,1), (2,2), (2,3) ])
                    , ((I, 3), [ (0,1), (1,1), (2,1), (3,1) ])

                    , ((J, 0), [ (1,0), (2,0), (2,1), (2,2) ])
                    , ((J, 1), [ (1,1), (1,2), (2,1), (3,1) ])
                    , ((J, 2), [ (2,0), (2,1), (2,2), (3,2) ])
                    , ((J, 3), [ (3,0), (3,1), (2,1), (1,1) ])

                    , ((L, 0), [ (1,2), (2,0), (2,1), (2,2) ])
                    , ((L, 1), [ (1,1), (3,2), (2,1), (3,1) ])
                    , ((L, 2), [ (2,0), (2,1), (2,2), (3,0) ])
                    , ((L, 3), [ (1,0), (3,1), (2,1), (1,1) ])

                    , ((O, 0), [ (1,1), (1,2), (2,1), (2,2) ])
                    , ((O, 1), [ (1,1), (1,2), (2,1), (2,2) ])
                    , ((O, 2), [ (1,1), (1,2), (2,1), (2,2) ])
                    , ((O, 3), [ (1,1), (1,2), (2,1), (2,2) ])

                    , ((S, 0), [ (2,0), (2,1), (1,1), (1,2) ])
                    , ((S, 1), [ (1,1), (2,1), (2,2), (3,2) ])
                    , ((S, 2), [ (3,0), (3,1), (2,1), (2,2) ])
                    , ((S, 3), [ (1,0), (2,0), (2,1), (3,1) ])

                    , ((T, 0), [ (2,0), (2,1), (2,2), (1,1) ])
                    , ((T, 1), [ (1,1), (2,1), (3,1), (2,2) ])
                    , ((T, 2), [ (2,0), (2,1), (2,2), (3,1) ])
                    , ((T, 3), [ (2,0), (1,1), (2,1), (3,1) ])

                    , ((Z, 0), [ (1,0), (1,1), (2,1), (2,2) ])
                    , ((Z, 1), [ (3,1), (2,1), (2,2), (1,2) ])
                    , ((Z, 2), [ (3,2), (3,1), (2,1), (2,0) ])
                    , ((Z, 3), [ (3,0), (2,0), (2,1), (1,1) ])
                    ]

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
