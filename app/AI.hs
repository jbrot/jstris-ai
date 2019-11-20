module AI where

import Control.Monad.State
import Parse

data AIState = AIState

defaultState :: AIState
defaultState = AIState

runAI :: Monad m => [[Square]] -> StateT AIState m [Action]
runAI _ = pure []
