{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Unlift where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Carrier.Internal.Interpret

import Control.Effect.Type.Unlift

import Control.Monad.Trans.Identity


data UnliftH

instance Carrier m
      => PrimHandler UnliftH (Unlift m) m where
  effPrimHandler (Unlift main) = main id
  {-# INLINEABLE effPrimHandler #-}

newtype UnliftC m a = UnliftC {
    unUnliftC :: m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

deriving via InterpretPrimC UnliftH (Unlift m) m
    instance Carrier m => Carrier (UnliftC m)
