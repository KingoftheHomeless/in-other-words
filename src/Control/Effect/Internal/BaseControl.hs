{-# LANGUAGE DerivingVia, MagicHash #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.BaseControl where

import Control.Effect.Carrier
import Control.Effect.Internal.Itself
import Control.Effect.Carrier.Internal.Interpret

import Control.Monad.Trans.Identity
import Control.Effect.Type.Internal.BaseControl

import GHC.Exts (Proxy#, proxy#)

data BaseControlH

instance Carrier m => PrimHandler BaseControlH (BaseControl m) m where
  effPrimHandler (GainBaseControl main) = return $ main (proxy# :: Proxy# (Itself m))
  {-# INLINE effPrimHandler #-}

newtype BaseControlC m a = BaseControlC {
    unBaseControlC :: m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

deriving via InterpretPrimC BaseControlH (BaseControl m) m
    instance Carrier m => Carrier (BaseControlC m)

