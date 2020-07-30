module Control.Effect.Primitive
  (-- Interpretation of primitive effects
    PrimHandler(..)
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

import Control.Effect.Internal.Union
import Control.Effect.Carrier.Internal.Interpret
