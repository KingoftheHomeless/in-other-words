{-# LANGUAGE DeriveFunctor, DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Optional where

import Control.Monad
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Carrier.Internal.Interpret

import Control.Effect.Type.Optional

import Data.Kind (Type)

newtype HoistOptionCall b a = HoistOptionCall (forall x. (a -> x) -> b x -> b x)
  deriving (Functor)

-- | A useful specialization of 'Optional' where the functor is
-- @'HoistOptionCall' b@. From this, you can derive
-- 'Control.Effect.Optional.hoistOption'.
type HoistOption (b :: Type -> Type) = Optional (HoistOptionCall b)

data HoistOptionH

instance Carrier m => PrimHandler HoistOptionH (HoistOption m) m where
  effPrimHandler (Optionally (HoistOptionCall b) m) = b id m
  {-# INLINEABLE effPrimHandler #-}

newtype HoistOptionC m a = HoistOptionC {
    unHoistOptionC :: m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

deriving via InterpretPrimC HoistOptionH (HoistOption m) m
    instance Carrier m => Carrier (HoistOptionC m)
