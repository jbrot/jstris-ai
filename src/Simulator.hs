module Simulator where

import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.State
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Random.Shuffle

import AI
import Tetris

emptyBoard :: Board
emptyBoard = V.replicate 200 Empty

pieceQueue :: RandomGen g => g -> [Block]
pieceQueue = runIdentity . evalRandT (fmap mconcat . sequence . repeat . shuffleM $ [ I, J, L, O, S, T, Z ])

startingState :: RandomGen g => g -> (GameState, [Block])
startingState g = (GameState emptyBoard (startingPosition active) Nothing queue, leftOver)
    where (active:queue, leftOver) = splitAt 6 . pieceQueue $ g

moveLeft :: ActiveBlock -> ActiveBlock
moveLeft (ActiveBlock b (r,c) rot) = ActiveBlock b (r, c - 1) rot

moveRight :: ActiveBlock -> ActiveBlock
moveRight (ActiveBlock b (r,c) rot) = ActiveBlock b (r, c + 1) rot

softDrop :: ActiveBlock -> ActiveBlock
softDrop (ActiveBlock b (r,c) rot) = ActiveBlock b (r + 1, c) rot

cycleActive :: (GameState, [Block]) -> (GameState, [Block])
cycleActive (GameState board _ h (n:q), nq:buffer) = (GameState board (startingPosition n) h (q <> [nq]), buffer)

rotateLeft :: ActiveBlock -> ActiveBlock
rotateLeft (ActiveBlock b p rot) = ActiveBlock b p ((rot + 3) `mod` 4)

rotateRight :: ActiveBlock -> ActiveBlock
rotateRight (ActiveBlock b p rot) = ActiveBlock b p ((rot + 1) `mod` 4) 

toggleHold :: (GameState, [Block]) -> (GameState, [Block])
toggleHold  (GameState board a Nothing q, b) = cycleActive (GameState board a (Just . kind $ a) q, b)
toggleHold  (GameState board a (Just k) q, b) = (GameState board (startingPosition k) (Just . kind $ a) q, b)

advance :: (GameState, [Block]) -> Action -> (GameState, [Block])
advance (g,b) MoveLeft = (g{active = moveLeft (active g)}, b)
advance (g,b) MoveRight = (g{active = moveRight (active g)}, b)
advance (g,b) SoftDrop = (g{active = softDrop (active g)}, b)
advance (g,b) HardDrop = cycleActive (g{board=dropBlock (board g) (active g)},b)
advance (g,b) RotateLeft = (g{active = rotateLeft (active g)}, b)
advance (g,b) RotateRight = (g{active = rotateRight (active g)}, b)
advance (g,b) Hold = toggleHold (g,b)

simulateAI :: (MonadIO m, RandomGen g) => g -> Int -> AIState -> m ()
simulateAI gen ct = evalStateT $ go ct st0
    where st0 = startingState gen
          disp :: MonadIO m => GameState -> m ()
          disp state = liftIO . (>> putStrLn "") . printBoard . addActiveBlock (board state) . active $ state
          go :: MonadIO m => Int -> (GameState, [Block]) -> StateT AIState m ()
          go 0 st = disp . fst $ st 
          go n (gs, q) = do
              let st = (gs{board = clearLines . board $ gs}, q)
              disp . fst $ st
              actions <- runAI . fst $ st
              go (n - 1) $ foldl advance st actions
