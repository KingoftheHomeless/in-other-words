{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Throw where

-- | An effect for throwing exceptions of type @e@.
newtype Throw e (m :: * -> *) (a :: *) where
  Throw :: e -> Throw e m a

