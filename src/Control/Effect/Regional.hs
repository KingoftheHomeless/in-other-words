{-# LANGUAGE DerivingVia #-}
module Control.Effect.Regional
  ( -- * Effects
    Regional(..)
  , Hoist

    -- * Actions
  , regional
  , hoist

    -- * Interpreters
  , HoistC(HoistC)
  , runHoist

  , HoistToFinalC
  , hoistToFinal

    -- * Threading utilities
  , threadRegionalViaOptional
  ) where

import Control.Effect
import Control.Effect.Carrier

import Control.Effect.Type.Regional
import Control.Effect.Type.Optional
import Control.Effect.Internal.Regional

-- | Execute a computation modified in some way, providing
-- the interpreter of @'Regional' s@ a constant to indicate
-- how the computation should be modified.
regional :: Eff (Regional s) m => s -> m a -> m a
regional s m = send (Regional s m)
{-# INLINE regional #-}

-- | Lift a natural transformation of a base monad to the
-- current monad.
hoist :: Eff (Hoist b) m => (forall x. b x -> b x) -> m a -> m a
hoist n = regional (HoistCall n)
{-# INLINE hoist #-}

type HoistToFinalC b = InterpretPrimC HoistToFinalH (Hoist b)

-- | Run a @'Hoist' m@ effect, where the base monad @m@ is the current monad.
--
-- @'Derivs' ('HoistC' m) = 'Hoist' m ': 'Derivs' m@
-- @'Prims'  ('HoistC' m) = 'Hoist' m ': 'Prims' m@
runHoist :: Carrier m
         => HoistC m a
         -> m a
runHoist = unHoistC
{-# INLINE runHoist #-}

-- | Run a @'Hoist' b@ effect, where the base monad @b@ is the final base monad.
--
-- @'Derivs' ('HoistToFinalC' b m) = 'Hoist' b ': 'Derivs' m@
-- @'Prims'  ('HoistToFinalC' b m) = 'Hoist' b ': 'Prims' m@
hoistToFinal :: ( MonadBaseControl b m
                , Carrier m
                )
             => HoistToFinalC b m a
             -> m a
hoistToFinal = interpretPrimViaHandler
{-# INLINE hoistToFinal #-}
