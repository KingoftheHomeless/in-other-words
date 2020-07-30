{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Alt where

-- | An effect corresponding to the 'Alternative' type class.
--
-- 'Control.Effect.Effly.Effly''s 'Alternative' instance is based
-- on this effect; by having access to 'Alt', you're able to use
-- '<|>' and 'empty' inside of effect handlers.
data Alt m a where
  Empty :: Alt m a
  Alt   :: m a -> m a -> Alt m a
