{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Tell where

data Tell s m a where
  Tell :: s -> Tell s m ()
