{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, DerivingVia, PatternSynonyms #-}
module Tetris.Board ( Square (..)
                    , Board, emptyBoard, fromSquares, toSquares, getSquare, isEmpty, printBoard
                    , canAddActiveBlock, validateAB, addActiveBlock
                    , complete, clearLines, addGarbageLines, hurryUp
                    ) where

import Data.Bits
import Data.Finite
import Data.Functor.Identity
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Vector.Unboxed.Sized (Vector)
import qualified Data.Vector.Unboxed.Sized as U
import qualified Data.Vector.Sized as V
import Data.Word
import Text.Printf

import Tetris.Block

data Square = Empty | Garbage | HurryUp
    deriving (Eq, Ord, Show)

type Board = (Vector 20 Bool,  Vector 20 Word32)

emptyRow :: Word32
emptyRow = (maxBound `shiftL` 18) .|. 255

fullRow :: Word32
fullRow = maxBound

rowMask :: Int -> Board -> Word32
rowMask r (_, contents)
  | r > 20 = fullRow
  | r <  0 = emptyRow
  | otherwise = contents `U.unsafeIndex` r

emptyBoard :: Board
emptyBoard = (U.replicate False, U.replicate emptyRow)

fromSquares :: V.Vector 20 (V.Vector 10 Square) -> Board
fromSquares v = (U.generate (\n -> (v `V.index` n) `V.index` 0 == HurryUp), U.generate (\n -> encodeRow (v `V.index` n)))
    where encodeRow :: V.Vector 10 Square -> Word32
          encodeRow = (.|. 255) . (`shiftL` 8) . foldr (\s v -> (v `shiftL` 1) .|. (if s == Empty then 0 else 1)) maxBound

toSquares :: Board -> V.Vector 20 (V.Vector 10 Square)
toSquares (hurry, contents) = V.generate genRow
  where genRow r = if hurry `U.index` r
                      then V.replicate HurryUp
                      else V.unfoldrN (\b -> (if (b .&. 1) == 0 then Empty else Garbage, b `shiftR` 1)) ((contents `U.index` r) `shiftR` 8)

getSquare :: Pos -> Board -> Square
getSquare (r,c) board
  | r >= 0 && r < 20 && (fst board) `U.unsafeIndex` r = HurryUp
  | ((rowMask r board) `shiftR` (8 + c)) .&. 1 == 0 = Empty
  | otherwise = Garbage

isEmpty :: Board -> Pos -> Bool
isEmpty b p = getSquare p b == Empty

-- Are all the spaces occupied by the ActiveBlock empty?
canAddActiveBlock :: Board -> ActiveBlock -> Bool 
canAddActiveBlock board ab = U.ifoldr chk True mask
    where (r,c) = pos ab
          mask = (rotMaskMap M.! (kind ab, rot ab))
          chk i m b = if (m `shiftL` (8 + c)) .&. (rowMask (r + (fromInteger . getFinite $ i)) board) == 0 then b else False

validateAB :: Board -> ActiveBlock -> Maybe ActiveBlock
validateAB b a = if canAddActiveBlock b a then Just a else Nothing

-- Replaces the squares in the board the ActiveBlock occupies with the appropriate remnants.
-- Does not check if spaces are overwritten.
addActiveBlock :: Board -> ActiveBlock -> Board
addActiveBlock (hurry, content) ab = (hurry, U.ifoldr upd content mask)
    where (r,c) = pos ab
          mask = U.map (`shiftL` (8 + c)) (rotMaskMap M.! (kind ab, rot ab))
          upd i m vc = if i' < 0 || i' >= 20 then vc else U.unsafeUpd vc [(i', (vc `U.unsafeIndex` i') .|. m)]
            where i' = r + (fromInteger . getFinite $ i)


complete :: Board -> Finite 20 -> Bool
complete (hurry,content) r = (content `U.index` r) + 1 == 0 && not (hurry `U.index` r)

clearLines :: Board -> (Int, Board)
clearLines board = foldr remove (0, board) . filter (complete board) . reverse $ [0..19] 
    where remove :: Finite 20 -> (Int, Board) -> (Int, Board)
          remove r (c, (hurry, contents)) = (c + 1, (hurry, cts' U.// [(0, emptyRow)]))
                where nind 0 = 0
                      nind i = fromInteger . getFinite $ if i <= r then i - 1 else i
                      upd = (U.generate nind) :: Vector 20 Int
                      cts' = U.backpermute contents upd

addGarbageLines :: Int -> Col -> Board -> Board
addGarbageLines n col (hurry,cts) = (hurry, cts' `U.unsafeUpd` [(x, grb_row) | x <- [endI - n .. endI - 1]])
    where endI = U.foldr (\b i -> if b then 0 else 1 + i) 0 hurry
          nind i = let i' = fromInteger . getFinite $ i in if i' > (endI - 1 - n) then i' else i' + n
          cts' = U.backpermute cts ((U.generate nind) :: Vector 20 Int)
          grb_row = complement (1 `shiftL` (col + 8))

-- Add `n` hurry up lines to the board.
hurryUp :: Int -> Board -> Board
hurryUp n = markHU . addGarbageLines n 32
    where markHU (h,c) = (h `U.unsafeUpd` [(x, True) | x <- [endI - n .. endI - 1]], c)
            where endI = U.foldr (\b i -> if b then 0 else 1 + i) 0 h

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
          sqChar :: Square -> Char
          sqChar Empty   = ' '
          sqChar Garbage = 'X'
          sqChar HurryUp = 'X'

{- These are the default colors on JStris. Now that the board no longer records remnant type,
    these no longer really have a use. I'm keeping them here, though, for posterity.
colorMap :: Map Block (Int, Int, Int)
colorMap = M.fromList [ (I, ( 15,155,215))
                      , (J, ( 33, 65,198))
                      , (L, (227, 91,  2))
                      , (O, (227,159,  2))
                      , (S, ( 89,177,  1))
                      , (T, (175, 41,138))
                      , (Z, (215, 15, 55))
                      ]
-}

rotMaskMap :: Map (Block, Rot) (Vector 4 Word32)
rotMaskMap = M.map posToMask rotMap

posToMask :: [Pos] -> Vector 4 Word32
posToMask [] = U.replicate 0
posToMask ((r,c):ps) = runIdentity $ U.ix (finite . toInteger $ r) (\v -> pure $ v .|. (1 `shiftL` c)) $ posToMask ps

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
