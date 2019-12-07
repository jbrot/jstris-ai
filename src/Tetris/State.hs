{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, DerivingVia, PatternSynonyms #-}
module Tetris.State where

import Control.Monad.Random
import Data.Bits
import Data.Maybe
import qualified Data.Vector.Unboxed.Sized as U

import Tetris.Action
import Tetris.Block
import Tetris.Board

-- Board -> Combo -> Cleared -> LinesSent
-- Combo counts consecutive clears, so if cleared > 0, then combo >= 1.
attackLines :: Board -> Int -> Int -> Int
attackLines board combo cleared = cboLines + clearedLines
    where cboLines = case (combo - 1) of
                       -1 -> 0
                       0  -> 0
                       1  -> 0
                       2  -> 1
                       3  -> 1
                       4  -> 1
                       5  -> 2
                       6  -> 2
                       7  -> 3
                       8  -> 3
                       9  -> 4
                       10 -> 4
                       11 -> 4
                       _ -> 5
          mask = 1023 `shiftL` 8
          clearedLines = if U.all (\r -> r .&. mask == 0) (snd board)
                            then 10
                            else case cleared of 
                                    0 -> 0
                                    1 -> 0
                                    2 -> 1
                                    3 -> 2
                                    4 -> 4
                                    _ -> undefined


data GameState = GameState { board :: Board
                           , active :: ActiveBlock
                           , held :: (Maybe Block)
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
