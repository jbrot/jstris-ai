{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingVia, PatternSynonyms #-}
module Tetris ( Block (I, J, L, O, S, T, Z)
              , Row, Col, Pos, Rot
              , ActiveBlock (ActiveBlock), kind, pos, rot, getCoords, startingPosition
              , Square (Empty, Garbage, Remnant, HurryUp)
              , Board, boardIndex, getSquare, isEmpty, canAddActiveBlock, validateAB, addActiveBlock, dropPosition, dropBlock, rotateBlock, complete, clearLines, addGarbageLines, printBoard
              , Action (MoveLeft, MoveRight, SoftDrop, HardDrop, RotateLeft, RotateRight, Hold), moveBlock, moveBlock'
              , GameState (GameState), board, active, held, queue, garbage, moveActive, moveActive', addActive, clearLines', addGarbage, reduceGarbage
              , pack, unpack
              ) where

import Control.Monad.Random
import Data.Finitary
import Data.Finitary.Finiteness
import Data.Finitary.PackInto (PackInto)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word
import GHC.Generics
import Text.Printf

data Block = I | J | L | O | S | T | Z
    deriving (Eq, Show, Generic, Finitary)
    deriving Ord via Finiteness Block

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
    deriving (Eq, Show, Generic, Finitary)
    deriving Ord via Finiteness Square

pack :: Square -> PackInto Square Word8
pack = fromFinite . toFinite
unpack :: PackInto Square Word8 -> Square
unpack = fromFinite . toFinite

type Board = Vector (PackInto Square Word8)

boardIndex :: Pos -> Int
boardIndex (r,c) = 10 * r + c

getSquare :: Pos -> Board -> PackInto Square Word8
getSquare p b = b V.! (boardIndex p)

isEmpty :: Board -> Pos -> Bool
isEmpty b (r,c) 
  | c <  0  = False
  | c >= 10 = False
  | r <  0  = True
  | r >= 20 = False
  | otherwise = b V.! boardIndex (r,c) == pack Empty

-- Are all the spaces occupied by the ActiveBlock empty?
canAddActiveBlock :: Board -> ActiveBlock -> Bool 
canAddActiveBlock board = and . fmap (isEmpty board) . getCoords

validateAB :: Board -> ActiveBlock -> Maybe ActiveBlock
validateAB b a = if canAddActiveBlock b a then Just a else Nothing

-- Replaces the squares in the board the ActiveBlock occupies with the appropriate remnants.
-- Does not check if spaces are overwritten.
addActiveBlock :: Board -> ActiveBlock -> Board
addActiveBlock board block = board V.// (filter (\(i,_) -> (i >= 0) && (i < 200)) updates)
    where updates = fmap (\p -> (boardIndex p, pack . Remnant . kind $ block)) . getCoords $ block

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
rotateBlock board dir def@(ActiveBlock k (r,c) rot) = listToMaybe . catMaybes . fmap (validateAB board) $ candidates
    where nrot = if dir then (rot + 1) `mod` 4 
                        else (rot + 3) `mod` 4
          kicks = if k == I then kickMap M.! (I, rot, dir)
                            else kickMap M.! (J, rot, dir)
          candidates = fmap (\(ro,co) -> ActiveBlock k (r + ro, c + co) nrot) kicks

complete :: Board -> Row -> Bool
complete b r = V.null . V.filter (\s -> s == pack Empty || s == pack HurryUp) .  V.slice (10 * r) 10 $ b

clearLines :: Board -> (Int, Board)
clearLines board = foldr remove (0, board) . filter (complete board) . reverse $ [0..19] 
    where remove :: Row -> (Int, Board) -> (Int, Board)
          remove r (c, b) = (c + 1, V.modify (\v -> do
              MV.move (MV.slice 10 (10 * r) v) (MV.slice 0 (10 * r) v) 
              MV.set (MV.slice 0 10 v) (pack Empty)) b)

addGarbageLines :: Int -> Col -> Board -> Board
addGarbageLines n c b = V.modify (\v -> do
    let total = maybe 20 (`div` 10) . V.findIndex (== pack HurryUp) $ b
        len = (total - n) * 10
    MV.move (MV.slice 0 len v) (MV.slice (10 * n) len v)
    MV.set (MV.slice len (10 * n) v) (pack Garbage)
    sequence . fmap (\r -> MV.write v (boardIndex (r,c)) (pack Empty)) $ [(total - n)..(total - 1)]
    pure ()) b

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
addGarbage g = fmap (\b -> g{board = b}) $ foldl update (pure $ board g) (garbage g)
  where update :: MonadRandom m => m Board -> Int -> m Board
        update b' ct = do
            cl <- getRandomR (0,9)
            pure . addGarbageLines ct cl =<< b'

reduceGarbage :: Int -> GameState -> GameState
reduceGarbage _ g@GameState{garbage = [] } = g
reduceGarbage c g@GameState{garbage = ct:gbs}
  | c >= ct  = reduceGarbage (c - ct) g{garbage = gbs}
  | otherwise = g{garbage = (ct - c):gbs}

printBoard :: Board -> IO ()
printBoard board = (>> return ()) . sequence . fmap (printRow board) $ [0..19]
    where printSquare :: PackInto Square Word8 -> IO ()
          printSquare s = let (r,g,b) = sqColor (unpack s) in printf "\x1b[48;2;%d;%d;%dm%c" r g b (sqChar . unpack $ s)
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
