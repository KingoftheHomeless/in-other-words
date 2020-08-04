module Control.Effect.Newtype
  ( -- * Wrapping
    WrapC
  , wrapWith

    -- * Unwrapping
  , EffNewtype(..)
  , WrapperOf

  , UnwrapC
  , unwrap

  , UnwrapTopC
  , unwrapTop

  ) where

import Control.Effect.Internal.Newtype
