{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, DerivingVia, PatternSynonyms #-}
module Tetris.State where

import Control.Monad.Random
import Data.Maybe

import Tetris.Action
import Tetris.Block
import Tetris.Board

data GameState = GameState { board :: Board
                           , active :: ActiveBlock
                           , held :: (Maybe Block)
                           , canHold :: Bool
                           , combo :: Int
                           , queue :: [Block]
                           , garbage :: [Int]
                           } deriving (Eq, Ord, Show)

newtype TransitionState = TransitionState (Bool, GameState)

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
