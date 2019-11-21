module AI where

import Control.Monad.State
import Tetris

data AIState = AIState Bool

-- Proof of Concept AI. Alternate dropping things on the left and right edge.
defaultState :: AIState
defaultState = AIState False

runAI :: Monad m => GameState -> StateT AIState m [Action]
runAI state = do
    let cols = fmap snd . getCoords . active $ state
    (AIState b) <- get
    put (AIState $ not b)
    if b 
       then pure $ replicate (minimum cols) MoveLeft <> [HardDrop]
       else pure $ replicate (10 - maximum cols) MoveRight <> [HardDrop]

possibleMoves :: GameState -> [ ([Action], Board) ]
possibleMoves (GameState board active _ _) = mconcat . fmap moves $ [0..3]
    where rotate :: ActiveBlock -> Int -> ActiveBlock
          rotate block@ActiveBlock{ rot = r } i = block{rot = (r + i) `mod` 4} 
          moves :: Int -> [([Action], Board)]
          moves i = fmap (\(as,b) -> (replicate i RotateRight <> as,b)) $ possibleTranslations board (rotate active i) 

possibleTranslations :: Board -> ActiveBlock -> [([Action], Board)]
possibleTranslations board block@ActiveBlock{ pos = (r, c) } = fmap (fmap $ dropBlock board) (left <> right)
    where cols = fmap snd .  getCoords $ block
          left = fmap (\x -> (replicate x MoveLeft, block{ pos = (r, c - x) })) [1..(minimum cols)]
          right = fmap (\x -> (replicate x MoveRight, block{ pos = (r, c - x) })) [0..(9 - maximum cols)]
