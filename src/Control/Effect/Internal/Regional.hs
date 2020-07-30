{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Regional where

import Control.Effect
import Control.Effect.Carrier

import Control.Effect.Type.Regional

import Control.Effect.Carrier.Internal.Interpret

import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity

newtype HoistCall b = HoistCall (forall x. b x -> b x)

-- | A useful specialization of 'Regional' where the
-- constant type is @'HoistCall' b@. From this,
-- you can derive 'hoist'.
type Hoist (b :: * -> *) = Regional (HoistCall b)

data HoistH

instance Carrier m => PrimHandler HoistH (Hoist m) m where
  effPrimHandler (Regional (HoistCall b) m) = b m
  {-# INLINE effPrimHandler #-}

data HoistToFinalH

newtype HoistC m a = HoistC {
    unHoistC :: m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

deriving via InterpretPrimC HoistH (Hoist m) m
    instance Carrier m => Carrier (HoistC m)

instance ( Carrier m
         , MonadBaseControl b m
         )
      => PrimHandler HoistToFinalH (Hoist b) m where
  effPrimHandler (Regional (HoistCall b) m) = control $ \lower -> b (lower m)
  {-# INLINE effPrimHandler #-}
