{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Alt where

-- | An effect corresponding to the
-- 'Control.Applicative.Alternative' type class.
--
-- 'Control.Effect.Effly''s 'Control.Applicative.Alternative' instance
-- is based on this effect; by having access to 'Alt', you're able to use
-- 'Control.Applicative.<|>' and 'Control.Applicative.empty' inside of effect
-- handlers.
--
-- Each 'Alt' interpreter's associated carrier
-- has an 'Control.Applicative.Alternative' instance based on
-- how it interprets 'Alt'. This means you can use
-- an 'Alt' interpreter to locally gain access to an 'Control.Applicative.Alternative'
-- instance inside of application code.
data Alt (m :: * -> *) a where
  Empty :: Alt m a
  Alt   :: m a -> m a -> Alt m a
