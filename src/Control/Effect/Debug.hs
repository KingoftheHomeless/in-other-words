-- | Utilities for debugging
module Control.Effect.Debug where

import Control.Effect.Carrier
import Data.Kind (Type)
import GHC.TypeLits

-- Type family needed to delay the TypeError until when 'debugEffects'
-- is used.
type family DebugEffects (m :: Type -> Type) :: k where
  DebugEffects m = TypeError (     'Text "Control.Effect.Debug.debugEffects"
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
debugEffects = errorWithoutStackTrace "debugEffects: impossible"
