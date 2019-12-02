{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module AI (NL, NNet, AIState (AIState), defaultState, parseAI, saveAI, stepAI, runAI) where

import Control.Monad.Random
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Serialize (encode, decode)
import Data.Singletons
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Num
import Data.Singletons.TypeLits
import Data.Type.Equality ((:~:)(..))
import qualified Data.Vector.Storable as V
import Grenade
import Numeric.LinearAlgebra.Static
import System.Random

import Tetris

-- Input: Board (200) + Queue (7 * 5 = 35) + Active (7) + Active Position (2) + Active Rotation (1) + Combo (1) + Incoming (1) = 247
-- Output: Left | Right | Rotate Left | Rotate Right | Drop (5)
type NL = '[ FullyConnected 247 256, Relu, FullyConnected 256 256, Relu, FullyConnected 256 5, Softmax ]
type NNet = Network NL '[ 'D1 247, 'D1 256, 'D1 256, 'D1 256, 'D1 256, 'D1 5, 'D1 5 ]

data AIState = AIState  { nn :: NNet
                        , scombo :: Int 
                        }
  deriving Show

defaultState :: IO AIState
defaultState = AIState <$> randomNetwork <*> pure 0

parseAI :: ByteString -> Either String AIState
parseAI s = AIState <$> decode s <*> pure 0

saveAI :: AIState -> ByteString
saveAI = encode . nn

sample :: (MonadRandom m, KnownNat n, (1 <=? n) ~ 'True) => R n -> m Int
sample v = fmap (go v) . getRandomR $ (0,1)
    where go :: forall n1. (KnownNat n1, (1 <=? n1) ~ 'True) => R n1 -> Double -> Int
          go vec v = if v < h
                        then 0
                        else case (SNat :: SNat n1) %- (SNat :: SNat 1) of
                               SNat -> case (SNat :: SNat 1) %<=? singByProxy t of
                                         STrue -> 1 + go t (v - h)
                                         SFalse -> 0
              where (h,t) = headTail vec

seedVector :: (KnownNat n, (1 <=? n) ~ 'True) => MonadRandom m => R n -> m (Int, R n)
seedVector v = fmap (\t -> (t, fromJust . create . flip V.unsafeUpd [(t, 1 / ((unwrap v) V.! t))] $ V.replicate (size v) 0)) (sample v)

apply :: MonadRandom m => NNet -> R 247 -> m (Int, Gradients NL)
apply nn v = fmap (fmap (fst . runGradient nn tape . S1D)) (seedVector o)
    where (tape, S1D o) = runNetwork nn (S1D v)

intToAct :: Int -> Action
intToAct = undefined

input :: Monad m => GameState -> StateT AIState m (R 247)
input = undefined

processDrop :: GameState -> AIState -> AIState
processDrop gs s = if lines > 0 then s{scombo = 1 + scombo s} else s{scombo = 0}
  where lines = fst . clearLines' . addActive . moveActive' HardDrop $ gs

stepAI :: (MonadRandom m) => GameState -> StateT AIState m (Action, Gradients NL)
stepAI state = do
    (anum, grad) <- join $ liftM2 apply (fmap nn get) (input state)
    let action = intToAct anum
    when (action == HardDrop) $ modify (processDrop state)
    pure  (action, grad)

runAI :: (MonadRandom m) => Int -> GameState -> StateT AIState m [(Action, Maybe (Gradients NL))]
runAI 0 s = modify (processDrop s) >> pure [(HardDrop, Nothing)]
runAI n s = do
    (a, g) <- stepAI s
    let s' = moveActive' a s
    case a of
      HardDrop -> pure [(a, Just g)]
      otherwise -> fmap ((:) (a, Just g)) (runAI (n - 1) s')
