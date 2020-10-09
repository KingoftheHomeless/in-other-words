module Control.Effect.Mask
  ( -- * Effects
    Mask(..)
  , MaskMode(..)

    -- * Actions
  , mask
  , mask_
  , uninterruptibleMask
  , uninterruptibleMask_

    -- * Interpretations
  , maskToIO

  , ignoreMask

    -- * Threading utilities
  , threadMaskViaClass

    -- * MonadMask
  , C.MonadMask

    -- * Carriers
  , MaskToIOC
  , IgnoreMaskC
  ) where

import Control.Effect
import Control.Effect.Primitive
import Control.Effect.Type.Mask

import Control.Monad.Catch (MonadMask)
import qualified Control.Monad.Catch as C

mask :: Eff Mask m => ((forall x. m x -> m x) -> m a) -> m a
mask main = send (Mask InterruptibleMask main)
{-# INLINE mask #-}

mask_ :: Eff Mask m => m a -> m a
mask_ main = mask $ \_ -> main
{-# INLINE mask_ #-}

uninterruptibleMask :: Eff Mask m => ((forall x. m x -> m x) -> m a) -> m a
uninterruptibleMask main = send (Mask UninterruptibleMask main)
{-# INLINE uninterruptibleMask #-}

uninterruptibleMask_ :: Eff Mask m => m a -> m a
uninterruptibleMask_ main = uninterruptibleMask $ \_ -> main
{-# INLINE uninterruptibleMask_ #-}

data MaskToIOH

instance ( Carrier m
         , MonadMask m
         )
      => PrimHandler MaskToIOH Mask m where
  effPrimHandler (Mask InterruptibleMask main)   = C.mask main
  effPrimHandler (Mask UninterruptibleMask main) = C.uninterruptibleMask main
  {-# INLINEABLE effPrimHandler #-}

type MaskToIOC = InterpretPrimC MaskToIOH Mask

-- | Run a 'Mask' effect by making use of the 'IO'-based 'Control.Exception.mask' and
-- 'Control.Exception.uninterruptibleMask'.
--
-- @'Derivs' ('MaskToIOC' m) = 'Mask' ': 'Derivs' m@
--
-- @'Prims'  ('MaskToIOC' m) = 'Mask' ': 'Prims' m@
maskToIO :: ( Carrier m
            , MonadMask m
            )
         => MaskToIOC m a
         -> m a
maskToIO = interpretPrimViaHandler
{-# INLINE maskToIO #-}

data IgnoreMaskH

instance Carrier m
      => Handler IgnoreMaskH Mask m where
  effHandler (Mask _ main) = main id
  {-# INLINEABLE effHandler #-}

type IgnoreMaskC = InterpretC IgnoreMaskH Mask

-- | Run a 'Mask' effect by ignoring it, providing no protection
-- against asynchronous exceptions.
--
-- @'Derivs' ('IgnoreMaskC' m) = 'Mask' ': 'Derivs' m@
--
-- @'Prims'  ('IgnoreMaskC' m) = 'Prims' m@
ignoreMask :: Carrier m
           => IgnoreMaskC m a
           -> m a
ignoreMask = interpretViaHandler
{-# INLINE ignoreMask #-}
