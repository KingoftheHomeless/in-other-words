{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Catch where

import Control.Effect.Type.Throw
import Control.Effect.Internal.Union

-- | An effect for catching exceptions of type @e@.
data Catch e :: Effect where
  Catch :: m a -> (e -> m a) -> Catch e m a

-- | A pseudo-effect for connected @'Throw' e@ and @'Catch' e@ effects.
--
-- @'Error' e@ should only ever be used inside of 'Control.Effect.Eff'
-- and 'Control.Effect.Effs' constraints. It is not a real effect!
-- See 'Bundle'.
type Error e = Bundle '[Throw e, Catch e]
