{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, DerivingVia, PatternSynonyms, RankNTypes, ScopedTypeVariables #-}
module Tetris.State ( GameState (..)
                    , TransitionState (..)
                    , moveActive, moveActive', addActive, clearLines'
                    , addGarbage, reduceGarbage
                    ) where

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
                           } deriving (Show)

data GSRecord = GSR { equal :: GameState -> GameState -> Bool
                    , comp  :: GameState -> GameState -> Ordering }
wrap :: (Eq a, Ord a) => (GameState -> a) -> GSRecord
wrap f = GSR (\g1 g2 -> f g1 == f g2) (\g1 g2 -> compare (f g1) (f g2))
compareList = [wrap board, wrap active, wrap held, wrap canHold, wrap combo, wrap garbage]

instance Eq GameState where
    g1 == g2 = and . fmap (\r -> equal r g1 g2) $ compareList
instance Ord GameState where
    compare g1 g2 = foldr cmp EQ compareList
        where cmp :: GSRecord -> Ordering -> Ordering
              cmp f b = let c = comp f g1 g2 in if c == EQ then b else c

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
