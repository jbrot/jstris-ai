module AI where

import Control.Monad.State
import Parse

data AIState = AIState Bool

-- Proof of Concept AI. Alternate dropping things on the left and right edge.
defaultState :: AIState
defaultState = AIState False

runAI :: Monad m => [[Square]] -> StateT AIState m [Action]
runAI f = do
    let frame = mconcat . tagArray $ f
    let sqFilter c = case c of
                    Empty -> False
                    Garbage -> False
                    otherwise -> True
    let rows = fmap (snd . fst) . filter (sqFilter . snd) $ frame
    if null rows 
       then pure []
       else do
         (AIState b) <- get
         put (AIState $ not b)
         if b then do
             let m = minimum rows
             pure (replicate m MoveLeft <> [HardDrop])
         else do
             let m = maximum rows
             pure (replicate (10 - m) MoveRight <> [HardDrop])
