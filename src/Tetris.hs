module Tetris ( Block (I, J, L, O, S, T, Z)
              , Row, Col, Pos, Rot
              , ActiveBlock (ActiveBlock), kind, pos, rot, getCoords
              , Square (Empty, Garbage, Remnant)
              , Board, boardIndex, getSquare, isEmpty, addActiveBlock, printBoard
              , GameState (GameState), board, active, held, queue
              , Action (MoveLeft, MoveRight, SoftDrop, HardDrop, RotateLeft, RotateRight, Hold)
              ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V
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

data Square = Empty | Garbage | Remnant Block
    deriving (Eq, Show)

type Board = Vector Square

boardIndex :: Pos -> Int
boardIndex (r,c) = 10 * r + c

getSquare :: Pos -> Board -> Square
getSquare p b = b V.! (boardIndex p)

isEmpty :: Board -> Pos -> Bool
isEmpty b p = maybe True (== Empty) $ b V.!? (boardIndex p)

addActiveBlock :: ActiveBlock -> Board -> Board
addActiveBlock block board = board V.// (filter (\(i,_) -> (i >= 0) && (i < 200)) updates)
    where updates = fmap (\p -> (boardIndex p, Remnant (kind block))) . getCoords $ block

data GameState = GameState { board :: Board
                           , active :: ActiveBlock
                           , held :: Maybe Block
                           , queue :: [Block]
                           }

data Action = MoveLeft | MoveRight | SoftDrop | HardDrop | RotateLeft | RotateRight | Hold

printBoard :: Board -> IO ()
printBoard board = (>> return ()) . sequence . fmap (printRow board) $ [0..19]
  where printSquare :: Square -> IO ()
        printSquare s = let (r,g,b) = sqColor s in printf "\x1b[48;2;%d;%d;%dm%c" r g b (sqChar s)
        printRow :: Board -> Row -> IO ()
        printRow b r = (>> printf "\x1b[0m\n") . sequence . fmap (printSquare . (\c -> getSquare (r, c) b)) $ [0..9]
        sqColor :: Square -> (Int, Int, Int)
        sqColor Empty = (0,0,0)
        sqColor Garbage = (115,115,115)
        sqColor (Remnant b) = colorMap M.! b
        sqChar :: Square -> Char
        sqChar Empty   = ' '
        sqChar Garbage = 'X'
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

                    , ((L, 0), [ (1,3), (2,0), (2,1), (2,2) ])
                    , ((L, 1), [ (1,1), (3,2), (2,1), (3,1) ])
                    , ((L, 2), [ (2,0), (2,1), (2,2), (3,0) ])
                    , ((L, 3), [ (1,0), (3,1), (2,1), (1,1) ])

                    , ((O, 0), [ (1,1), (1,2), (2,1), (2,2) ])
                    , ((O, 1), [ (1,1), (1,2), (2,1), (2,2) ])
                    , ((O, 2), [ (1,1), (1,2), (2,1), (2,2) ])
                    , ((O, 3), [ (1,1), (1,2), (2,1), (2,2) ])

                    , ((S, 0), [ (2,0), (2,1), (1,1), (1,3) ])
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
