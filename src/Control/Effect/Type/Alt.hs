module Control.Effect.Type.Alt where

data Alt m a where
  Empty :: Alt m a
  Alt   :: m a -> m a -> Alt m a
