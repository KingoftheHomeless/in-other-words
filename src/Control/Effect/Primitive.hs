module Control.Effect.Primitive
  ( -- * Primitive effects
    Carrier(Derivs, Prims)

    -- * Interpretation of primitive effects
  , EffPrimHandler

    -- ** 'interpretPrimSimple'
  , interpretPrimSimple

    -- ** 'interpretPrimViaHandler'
  , interpretPrimViaHandler
  , PrimHandler(..)

    -- ** 'interpretPrim'
  , interpretPrim


    -- * Threading primitive effects
  , Threads(..)
  , ThreadsEff(..)

    -- * Carriers
  , InterpretPrimSimpleC
  , InterpretPrimC
  , InterpretPrimReifiedC
  ) where

import Control.Effect.Internal
import Control.Effect.Internal.Union
import Control.Effect.Carrier.Internal.Interpret
