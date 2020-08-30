module Control.Effect.Select
  ( -- * Effect
    Select(..)

    -- * Actions
  , select

    -- * Interpretations
  , runSelect

  , runSelectFast

    -- * Threading constraints
  , SelectThreads
  , SelectFastThreads

    -- * Carriers
  , SelectC
  , SelectFastC
  ) where

import Control.Effect

import Control.Effect.Internal.Select

-- | Perform a search: capture the continuation
-- of the program, so that you may test values of @a@ and observe
-- what corresponding @s@ each value would result in
-- at the end of the program (which may be seen as the evaluation of @a@).
-- When you find a satisfactory @a@, you may return the associated @r@.
--
-- The way higher-order actions interact with the continuation depends
-- on the interpretation of 'Select'. In general, you cannot expect to interact
-- with the continuation in any meaningful way: for example, you should not
-- assume that you will be able to catch an exception thrown at some point in
-- the future of the computation by using 'Control.Effect.Error.catch' on the
-- continuation.
select :: Eff (Select s) m
       => (forall r. (a -> m (s, r)) -> m r) -> m a
select main = send (Select main)
{-# INLINE select #-}
