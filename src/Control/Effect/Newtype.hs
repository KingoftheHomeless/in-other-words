module Control.Effect.Newtype
  ( -- * Wrapping
    wrapWith

    -- * Unwrapping
  , EffNewtype(..)
  , WrapperOf

  , unwrap

  , unwrapTop

    -- * Carriers
  , WrapC
  , UnwrapC
  , UnwrapTopC
  ) where

import Control.Effect.Internal.Newtype
