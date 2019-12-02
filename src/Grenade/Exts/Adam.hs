{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}
module Grenade.Exts.Adam where

import Grenade
import Grenade.Exts.Gradient
import Grenade.Exts.Layer

data Adam t = Adam { alpha :: t
                   , beta1 :: t
                   , beta2 :: t
                   , epsilon :: t
                   , mom :: t
                   , vel :: t
                   , time :: Int
                   }

defAdam :: (Fractional t) => Adam t
defAdam = Adam (rtf 0.01) (rtf 0.9) (rtf 0.999) (rtf 1e-8) (rtf 0) (rtf 0) 0
    where rtf :: Fractional t => Double -> t
          rtf = realToFrac

runAdam :: (All UpdateLayerRaw layers, Floating (Gradients layers)) => Adam (Gradients layers) -> Gradients layers -> Network layers shapes -> (Adam (Gradients layers), Network layers shapes)
runAdam a g n = (a{mom = m, vel = v, time = t}, applyRaw del n)
  where t = 1 + (time a)
        m = (beta1 a) * (mom a) + (1 - beta1 a) * g
        v = (beta2 a) * (vel a) + (1 - beta2 a) * g * g
        at = (alpha a) * sqrt (1 - (beta2 a)^t) / (1 - (beta1 a)^t)
        del = -at * m / (sqrt v + epsilon a)
