{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Cut where

-- | An effect to delimit backtracking within nondeterministic contexts.
data Cut m a where
  Cutfail :: Cut m a
  Call    :: m a -> Cut m a
