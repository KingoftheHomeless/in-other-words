{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.AtomicState where

-- | An effect for atomically reading and writing a piece of state.
--
-- Convention: the interpreter for the @AtomicState@ action must force
-- the resulting tuple of the function, but not the end state or returned value.
data AtomicState s m a where
  AtomicState :: (s -> (s, a)) -> AtomicState s m a
  AtomicGet   :: AtomicState s m s
