{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Throw where

newtype Throw e m a where
  Throw :: e -> Throw e m a

