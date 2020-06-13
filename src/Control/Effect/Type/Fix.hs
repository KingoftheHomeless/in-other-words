module Control.Effect.Type.Fix where

newtype Fix m a where
  Fix :: (a -> m a) -> Fix m a
