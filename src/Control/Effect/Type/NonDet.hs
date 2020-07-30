{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.NonDet where

data NonDet m a where
  FromList :: [a] -> NonDet m a
