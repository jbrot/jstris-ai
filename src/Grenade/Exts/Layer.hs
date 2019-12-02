{-# LANGUAGE ConstraintKinds, DataKinds, KindSignatures, TypeFamilies, TypeOperators #-}
module Grenade.Exts.Layer where

import Data.Singletons.TypeLits
import GHC.Types (Constraint)
import Grenade
import Grenade.Exts.Gradient

class UpdateLayer x => UpdateLayerRaw x where
    runUpdateRaw :: Gradient x -> x -> x

instance (KnownNat i, KnownNat o) => UpdateLayerRaw (FullyConnected i o) where
    runUpdateRaw d (FullyConnected a b) = FullyConnected (d + a) b
instance UpdateLayerRaw (Relu) where
    runUpdateRaw _ _ = Relu
instance UpdateLayerRaw (Softmax) where
    runUpdateRaw _ _ = Softmax

type family All (c :: * -> Constraint) (as :: [*]) :: Constraint where
    All c '[] = ()
    All c (a ': as) = (c a, All c as)

applyRaw :: All UpdateLayerRaw layers => Gradients layers -> Network layers shapes -> Network layers shapes
applyRaw GNil NNil = NNil
applyRaw (g :/> gs) (n :~> ns) = (runUpdateRaw g n :~> ns)
