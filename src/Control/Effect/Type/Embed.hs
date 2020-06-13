module Control.Effect.Type.Embed where

import Control.Effect.Internal
import Control.Effect.Internal.Utils

newtype Embed b m a where
  Embed :: b a -> Embed b m a

embed :: Eff (Embed b) m => b a -> m a
embed = send .# Embed
{-# INLINE embed #-}
