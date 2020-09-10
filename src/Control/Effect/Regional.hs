{-# LANGUAGE DerivingVia #-}
module Control.Effect.Regional
  ( -- * Effects
    Regional(..)
  , Hoist

    -- * Actions
  , regionally
  , hoist

    -- * Interpretations
  , runHoist

  , hoistToFinal

    -- * Threading utilities
  , threadRegionalViaOptional

    -- * Combinators for 'Algebra's
    -- Intended to be used for custom 'Carrier' instances when
    -- defining 'algPrims'.
  , powerAlgHoist
  , powerAlgHoistFinal

    -- * Carriers
  , HoistC
  , HoistToFinalC
  ) where

import Control.Effect
import Control.Effect.Carrier

import Control.Effect.Type.Regional
import Control.Effect.Type.Optional
import Control.Effect.Internal.Regional

import Control.Monad.Trans.Control (control)

-- | Execute a computation modified in some way, providing
-- the interpreter of @'Regional' s@ a constant to indicate
-- how the computation should be modified.
regionally :: Eff (Regional s) m => s -> m a -> m a
regionally s m = send (Regionally s m)
{-# INLINE regionally #-}

-- | Lift a natural transformation of a base monad to the
-- current monad.
hoist :: Eff (Hoist b) m => (forall x. b x -> b x) -> m a -> m a
hoist n = regionally (HoistCall n)
{-# INLINE hoist #-}

type HoistToFinalC b = InterpretPrimC HoistToFinalH (Hoist b)

-- | Run a @'Hoist' m@ effect, where the base monad @m@ is the current monad.
--
-- @'Derivs' ('HoistC' m) = 'Hoist' m ': 'Derivs' m@
--
-- @'Prims'  ('HoistC' m) = 'Hoist' m ': 'Prims' m@
runHoist :: Carrier m
         => HoistC m a
         -> m a
runHoist = unHoistC
{-# INLINE runHoist #-}

-- | Run a @'Hoist' b@ effect, where the base monad @b@ is the final base monad.
--
-- @'Derivs' ('HoistToFinalC' b m) = 'Hoist' b ': 'Derivs' m@
--
-- @'Prims'  ('HoistToFinalC' b m) = 'Hoist' b ': 'Prims' m@
hoistToFinal :: ( MonadBaseControl b m
                , Carrier m
                )
             => HoistToFinalC b m a
             -> m a
hoistToFinal = interpretPrimViaHandler
{-# INLINE hoistToFinal #-}

-- | Strengthen an @'Algebra' p m@ by adding a @'Hoist' m@ handler
powerAlgHoist :: forall m p a
               . Algebra' p m a
              -> Algebra' (Hoist m ': p) m a
powerAlgHoist alg = powerAlg alg $ \(Regionally (HoistCall n) m) -> n m
{-# INLINE powerAlgHoist #-}

-- | Strengthen an @'Algebra' p m@ by adding a @'Hoist' b@ handler, where
-- @b@ is the final base monad.
powerAlgHoistFinal :: forall b m p a
                    . MonadBaseControl b m
                   => Algebra' p m a
                   -> Algebra' (Hoist b ': p) m a
powerAlgHoistFinal alg = powerAlg alg $ \case
  Regionally (HoistCall n) m -> control $ \lower -> n (lower m)
{-# INLINE powerAlgHoistFinal #-}
