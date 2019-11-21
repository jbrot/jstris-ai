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
