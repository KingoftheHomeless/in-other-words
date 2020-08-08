module Control.Effect.Optional
  ( -- * Effects
    Optional(..)
  , HoistOption
  , HoistOptionCall(..)

    -- * Actions
  , optionally
  , hoistOption

    -- * Interpretations
  , runHoistOption

  , hoistOptionToFinal

    -- * Threading utilities
  , threadOptionalViaBaseControl

    -- * Carriers
  , HoistOptionC
  , HoistOptionToFinalC
  ) where

import Control.Monad
import Control.Monad.Trans.Control

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Internal.Optional

import Control.Effect.Type.Internal.BaseControl
import Control.Effect.Type.Optional


-- | Execute the provided computation, providing the
-- interpretation of @'Optional' s@ the option to execute
-- it in full or in part.
optionally :: Eff (Optional s) m => s a -> m a -> m a
optionally s m = send (Optionally s m)
{-# INLINE optionally #-}

-- | Hoist a natural transformation of the base monad into the current
-- monad, equipped with the option to execute the provided computation
-- in full or in part.
hoistOption :: Eff (HoistOption b) m
            => (forall x. (a -> x) -> b x -> b x)
            -> m a -> m a
hoistOption n = optionally (HoistOptionCall n)
{-# INLINE hoistOption #-}

-- | Runs a @'HoistOption' m@ effect, where the base monad
-- @m@ is the current monad.
--
-- @'Derivs' ('HoistOptionC' m) = 'HoistOption' m ': 'Derivs' m@
-- @'Prims'  ('HoistOptionC' m) = 'HoistOption' m ': 'Prims' m@
runHoistOption :: Carrier m
               => HoistOptionC m a
               -> m a
runHoistOption = unHoistOptionC
{-# INLINE runHoistOption #-}

data HoistOptionToFinalH

instance ( Carrier m
         , MonadBaseControl b m
         )
      => PrimHandler HoistOptionToFinalH (HoistOption b) m where
  effPrimHandler (Optionally (HoistOptionCall b) m) =
    join $ liftBaseWith $ \lower ->
      b pure (restoreM <$> lower m)
  {-# INLINE effPrimHandler #-}

type HoistOptionToFinalC b = InterpretPrimC HoistOptionToFinalH (HoistOption b)

-- | Runs a @'HoistOption' b@ effect, where the base monad
-- @b@ is the final base monad.
--
-- @'Derivs' ('HoistOptionToFinalC' b m) = 'HoistOption' b ': 'Derivs' m@
-- @'Prims'  ('HoistOptionToFinalC' b m) = 'HoistOption' b ': 'Prims' m@
hoistOptionToFinal :: ( MonadBaseControl b m
                      , Carrier m
                      )
                   => HoistOptionToFinalC b m a
                   -> m a
hoistOptionToFinal = interpretPrimViaHandler
{-# INLINE hoistOptionToFinal #-}
