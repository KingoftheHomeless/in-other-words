{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Embed where

import Data.Kind (Type)

-- | An effect for embedding actions of a base monad into the current one.
newtype Embed b (m :: Type -> Type) a where
  Embed :: { unEmbed :: b a } -> Embed b m a
