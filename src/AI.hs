-- The actual tetris AI.
-- Heavily inspired by Lee Yiyuan's AI (https://github.com/LeeYiyuan/tetrisai).
module AI where

import Control.Monad.State
import Data.List

import Tetris

data AIState = AIState
  deriving Show

defaultState :: AIState
defaultState = AIState

runAI :: MonadIO m => GameState -> StateT AIState m [Action]
runAI state = pure . (<> [ HardDrop ]) . fst . maximumBy (\a b -> compare (snd a) (snd b)) . fmap (recurseScore 1) . possibleMoves $ state
    where recurseScore :: Int -> ([Action], Board) -> ([Action], Float)
          recurseScore 0 x = fmap score x
          recurseScore n (as, b) = ((,) as) . maximum . fmap (snd . recurseScore (n - 1)) . possibleMoves . nextState state $ b
          nextState :: GameState -> Board -> GameState
          nextState (GameState _ _ h (q:qs) g) b = GameState b (startingPosition q) h qs g

possibleMoves :: GameState -> [ ([Action], Board) ]
possibleMoves (GameState board active _ _ _) = mconcat . fmap moves $ [0..3]
    where rotate :: ActiveBlock -> Int -> ActiveBlock
          rotate block@ActiveBlock{ rot = r } i = block{rot = (r + i) `mod` 4} 
          moves :: Int -> [([Action], Board)]
          moves i = fmap (\(as,b) -> (replicate i RotateRight <> as,b)) $ possibleTranslations board (rotate active i) 

possibleTranslations :: Board -> ActiveBlock -> [([Action], Board)]
possibleTranslations board block@ActiveBlock{ pos = (r, c) } = fmap (fmap $ dropBlock board) (left <> right)
    where cols = fmap snd .  getCoords $ block
          left = fmap (\x -> (replicate x MoveLeft, block{ pos = (r, c - x) })) [1..(minimum cols)]
          right = fmap (\x -> (replicate x MoveRight, block{ pos = (r, c + x) })) [0..(9 - maximum cols)]

aggregateHeight :: Board -> Int
aggregateHeight board = sum (height board <$> [0..9])

height :: Board -> Col -> Int
height board c = (20 -) . minimum . (<> [20]) . filter (\r -> getSquare (r,c) board /= Empty) $ [0..19]

completeLines :: Board -> Int
completeLines board = length . filter complete $ [0..19]
    where complete :: Row -> Bool
          complete r = null . filter (\c -> getSquare (r,c) board == Empty) $ [0..9]

holes :: Board -> Int
holes board = sum (colHoles <$> [0..9])
    where colHoles :: Col -> Int
          colHoles c = length . filter (\r -> r > (20 - height board c) && getSquare (r,c) board == Empty) $ [0..19]

bumpiness :: Board -> Int
bumpiness board = sum . fmap (\c -> abs (height board c - height board (c + 1))) $ [0..8]

-- See https://codemyroad.wordpress.com/2013/04/14/tetris-ai-the-near-perfect-player/
score :: Board -> Float
score board = (-0.510066 * itf (aggregateHeight board)) + (0.760666 * itf (completeLines board)) + (-0.35663 * itf (holes board)) + (-0.184483 * itf (bumpiness board))
  where itf = fromInteger . toInteger
