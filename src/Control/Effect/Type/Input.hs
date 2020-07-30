{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Input where

data Input i m a where
  Input :: Input i m i
