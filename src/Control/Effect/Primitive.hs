module Control.Effect.Primitive
  ( -- * Carrier class
    Carrier(..)

    -- * Interpretation of primitive effects
  , EffPrimHandler
  , PrimHandler(..)
  , InterpretPrimC
  , interpretPrimViaHandler

  , InterpretPrimReifiedC
  , ReifiesPrimHandler
  , interpretPrim

  , interpretPrimSimple

    -- * Threading primitive effects
  , Threads(..)
  , ThreadsEff(..)
  ) where

import Control.Effect.Internal
import Control.Effect.Internal.Union
import Control.Effect.Carrier.Internal.Interpret
