{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Grenade.Exts.Gradient where

import Data.Singletons.TypeLits
import Grenade
import Numeric.LinearAlgebra.Static

instance (KnownNat i, KnownNat o) => Num (FullyConnected' i o) where
    (FullyConnected' a b) + (FullyConnected' a2 b2) = FullyConnected' (a + a2) (b + b2)
    (FullyConnected' a b) * (FullyConnected' a2 b2) = FullyConnected' (a * a2) (b * b2)
    abs (FullyConnected' a b) = FullyConnected' (abs a) (abs b)
    signum (FullyConnected' a b) = FullyConnected' (signum a) (signum b)
    fromInteger n = FullyConnected' (fromInteger n) (fromInteger n)
    negate (FullyConnected' a b) = FullyConnected' (negate a) (negate b)
instance (KnownNat i, KnownNat o) => Fractional (FullyConnected' i o) where
    recip (FullyConnected' a b) = FullyConnected' (recip a) (recip b)
    fromRational r = FullyConnected' (fromRational r) (fromRational r)
instance (KnownNat i, KnownNat o) => Floating (FullyConnected' i o) where
    pi = FullyConnected' pi pi
    exp (FullyConnected' a b) = FullyConnected' (exp a) (exp b)
    log (FullyConnected' a b) = FullyConnected' (log a) (log b)
    sin (FullyConnected' a b) = FullyConnected' (sin a) (sin b)
    cos (FullyConnected' a b) = FullyConnected' (cos a) (cos b)
    asin (FullyConnected' a b) = FullyConnected' (asin a) (asin b)
    acos (FullyConnected' a b) = FullyConnected' (acos a) (acos b)
    atan (FullyConnected' a b) = FullyConnected' (atan a) (atan b)
    sinh (FullyConnected' a b) = FullyConnected' (sinh a) (sinh b)
    cosh (FullyConnected' a b) = FullyConnected' (cosh a) (cosh b)
    asinh (FullyConnected' a b) = FullyConnected' (asinh a) (asinh b)
    acosh (FullyConnected' a b) = FullyConnected' (acosh a) (acosh b)
    atanh (FullyConnected' a b) = FullyConnected' (atanh a) (atanh b)

instance Num (Gradients '[]) where
    GNil + GNil = GNil
    GNil * GNil = GNil
    abs GNil = GNil
    signum GNil = GNil
    fromInteger n = GNil
    negate GNil = GNil
instance Fractional (Gradients '[]) where
    fromRational r = GNil
    recip GNil = GNil
instance Floating (Gradients '[]) where
    pi = GNil
    exp GNil = GNil
    log GNil = GNil
    sin GNil = GNil
    cos GNil = GNil
    asin GNil = GNil
    acos GNil = GNil
    atan GNil = GNil
    sinh GNil = GNil
    cosh GNil = GNil
    asinh GNil = GNil
    acosh GNil = GNil
    atanh GNil = GNil

instance (Num (Gradients as), Num (Gradient a), UpdateLayer a) => Num (Gradients (a ': as)) where
    (a :/> b) + (a2 :/> b2) = (a + a2) :/> (b + b2)
    (a :/> b) * (a2 :/> b2) = (a * a2) :/> (b * b2)
    abs (a :/> b) = (abs a) :/> (abs b)
    signum (a :/> b) = (signum a) :/> (signum b)
    fromInteger n = (fromInteger n) :/> (fromInteger n)
    negate (a :/> b) = (negate a) :/> (negate b)
instance (Fractional (Gradients as), Fractional (Gradient a), UpdateLayer a) => Fractional (Gradients (a ': as)) where
    fromRational r = (fromRational r) :/> (fromRational r)
    recip (a :/> b) = (recip a) :/> (recip b)
instance (Floating (Gradients as), Floating (Gradient a), UpdateLayer a) => Floating (Gradients (a ': as)) where
    pi = pi :/> pi
    exp (a :/> b) = (exp a) :/> (exp b)
    log (a :/> b) = (log a) :/> (log b)
    sin (a :/> b) = (sin a) :/> (sin b)
    cos (a :/> b) = (cos a) :/> (cos b)
    asin (a :/> b) = (asin a) :/> (asin b)
    acos (a :/> b) = (acos a) :/> (acos b)
    atan (a :/> b) = (atan a) :/> (atan b)
    sinh (a :/> b) = (sinh a) :/> (sinh b)
    cosh (a :/> b) = (cosh a) :/> (cosh b)
    asinh (a :/> b) = (asinh a) :/> (asinh b)
    acosh (a :/> b) = (acosh a) :/> (acosh b)
    atanh (a :/> b) = (atanh a) :/> (atanh b)

instance Num () where
    () + () = ()
    () * () = ()
    abs () = ()
    signum () = ()
    fromInteger n = ()
    negate () = ()
instance Fractional () where
    fromRational r = ()
    recip () = ()
instance Floating () where
    pi = ()
    exp () = ()
    log () = ()
    sin () = ()
    cos () = ()
    asin () = ()
    acos () = ()
    atan () = ()
    sinh () = ()
    cosh () = ()
    asinh () = ()
    acosh () = ()
    atanh () = ()
