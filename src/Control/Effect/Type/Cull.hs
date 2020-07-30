{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Cull where

-- | An effect for culling nondeterministic computations.
newtype Cull m a where
  Cull :: m a -> Cull m a
