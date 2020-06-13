module Control.Effect.Reader
  (
    module Control.Effect.Type.Reader
  , ask
  , local
  ) where

import Control.Effect.Type.Reader
import Control.Effect.Internal

ask :: Eff (Reader i) m => m i
ask = send Ask
{-# INLINE ask #-}

local :: Eff (Reader i) m => (i -> i) -> m a -> m a
local f m = send (Local f m)
{-# INLINE local #-}
