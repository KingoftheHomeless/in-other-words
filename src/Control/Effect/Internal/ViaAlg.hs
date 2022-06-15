module Control.Effect.Internal.ViaAlg where

import Control.Effect.Internal.Union
import Data.Coerce
import Data.Kind (Type)

type RepresentationalT = RepresentationalEff

newtype ViaAlg (s :: Type) (e :: Effect) m a = ViaAlg {
    unViaAlg :: m a
  }
  deriving (Functor, Applicative, Monad)

newtype ReifiedEffAlgebra e m = ReifiedEffAlgebra (forall x. e m x -> m x)

viaAlgT :: forall s e t m a. RepresentationalT t => t m a -> t (ViaAlg s e m) a
viaAlgT = coerce
{-# INLINE viaAlgT #-}

unViaAlgT :: forall s e t m a. RepresentationalT t => t (ViaAlg s e m) a -> t m a
unViaAlgT = coerce
{-# INLINE unViaAlgT #-}

mapViaAlgT :: forall s e t m n a b
            . RepresentationalT t
           => (t m a -> t n b)
           -> t (ViaAlg s e m) a
           -> t (ViaAlg s e n) b
mapViaAlgT = coerce
{-# INLINE mapViaAlgT #-}

mapUnViaAlgT :: forall s e t m n a b
             . RepresentationalT t
             => (t (ViaAlg s e m) a -> t (ViaAlg s e n) b)
             -> t m a
             -> t n b
mapUnViaAlgT = coerce
{-# INLINE mapUnViaAlgT #-}
