{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.State where

data State s m a where
  Get :: State s m s
  Put :: s -> State s m ()
