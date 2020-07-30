-- | Utilities for debugging
module Control.Effect.Debug where

import Control.Effect.Carrier
import GHC.TypeLits

type family DebugEffects (m :: * -> *) :: k where
  DebugEffects m = TypeError (     'Text "Control.Effect.Debug.debugStack"
                             ':$$: 'Text "Derivs: " ':<>: 'ShowType (Derivs m)
                             ':$$: 'Text "Prims:  " ':<>: 'ShowType (Prims m)
                             ':$$: 'Text "Carrier is:"
                             ':$$: 'Text "\t" ':<>: 'ShowType m
                   )

-- | A placeholder action of @m@ that causes a compile-time error
-- that tells you the derived and primitive effects of @m@.
--
-- Doesn't work when @m@ is polymorphic.
debugEffects :: DebugEffects m
             => m a
debugEffects = undefined
