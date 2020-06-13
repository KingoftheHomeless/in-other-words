{-# LANGUAGE BangPatterns #-}
module Control.Effect.Type.State where

import Control.Effect.Internal

data State s m a where
  State :: (s -> (s, a)) -> State s m a
  Get   :: State s m s

state :: Eff (State s) m => (s -> (s, a)) -> m a
state = send . State

get :: Eff (State s) m => m s
get = send Get

put :: Eff (State s) m => s -> m ()
put s = state $ \_ -> (s, ())

modify :: Eff (State s) m => (s -> s) -> m ()
modify f = state $ \s -> (f s, ())

modify' :: Eff (State s) m => (s -> s) -> m ()
modify' f = state $ \s -> let !s' = f s in (s', ())
