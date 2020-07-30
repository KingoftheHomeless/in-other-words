{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Select where

-- | An effect for backtracking search.
data Select s m a where
  Select :: (forall r. (a -> m (s, r)) -> m r) -> Select s m a
