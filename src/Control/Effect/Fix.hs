module Control.Effect.Fix
  ( -- * Effects
    Fix(..)
  , module Control.Monad.Fix

    -- * Interpretations
  , fixToFinal

    -- * Threading utilities
  , threadFixViaClass

    -- * Carriers
  , FixToFinalC
  ) where

import Control.Monad.Fix

import Control.Effect
import Control.Effect.Primitive
import Control.Effect.Type.Fix

data FixToFinalH

instance (Carrier m, MonadFix m)
      => PrimHandler FixToFinalH Fix m where
  effPrimHandler (Fix f) = mfix f
  {-# INLINE effPrimHandler #-}

type FixToFinalC = InterpretPrimC FixToFinalH Fix

-- | Run a 'Fix' effect if the final monad and
-- all carriers transforming it are 'MonadFix'.
--
-- @'Derivs' (FixToFinalC m) = 'Fix' ': 'Derivs' m@
--
-- @'Prims'  (FixToFinalC m) = 'Fix' ': 'Prims' m@
fixToFinal :: ( Carrier m
              , MonadFix m
              )
           => FixToFinalC m a
           -> m a
fixToFinal = interpretPrimViaHandler
{-# INLINE fixToFinal #-}
