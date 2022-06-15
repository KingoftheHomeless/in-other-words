{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Throw where

import Data.Kind (Type)

-- | An effect for throwing exceptions of type @e@.
newtype Throw e (m :: Type -> Type) (a :: Type) where
  Throw :: e -> Throw e m a

