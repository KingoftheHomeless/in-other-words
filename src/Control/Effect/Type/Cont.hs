{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Cont where

-- | An effect for abortive continuations.
data Cont m a where
  CallCC :: ((forall b. a -> m b) -> m a) -> Cont m a
