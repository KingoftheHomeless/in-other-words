module Control.Effect.Cont
  ( -- * Effects
    Cont(..)

    -- * Actions
  , callCC

    -- * Interpretations
  , runCont

  , runContFast

    -- * Threading constraints
  , ContThreads
  , ContFastThreads

    -- * Carriers
  , ContC
  , ContFastC
  ) where

import Control.Effect
import Control.Effect.Internal.Cont

import Control.Effect.Internal.Utils

import qualified Control.Monad.Trans.Cont as C
import Control.Monad.Trans.Free.Church.Alternate

-- | Call with current continuation. The argument computation is provided
-- the /continuation/ of the program at the point that 'callCC' was invoked.
-- If the continuation is executed, then control will immediately abort
-- and jump to the point 'callCC' was invoked, which will then return
-- argument provided to the continuation.
--
-- The way higher-order actions interact with the continuation depends
-- on the interpretation of 'Cont'. In general, you cannot expect to interact
-- with the continuation in any meaningful way: for example, you should not
-- assume that you will be able to catch an exception thrown at some point in
-- the future of the computation by using 'Control.Effect.Error.catch' on the
-- continuation.
callCC :: Eff Cont m
       => ((forall b. a -> m b) -> m a) -> m a
callCC main = send (CallCC main)
{-# INLINE callCC #-}

-- | Run a 'Cont' effect.
runCont :: forall m a p
         . ( Carrier m
           , Threaders '[ContThreads] m p
           )
        => ContC (m a) m a -> m a
runCont =
    foldFreeT
      id
      (\c -> \case
        Jump a -> a
        GetCont -> c $ Left (c . Right)
      )
  .# unContC
{-# INLINE runCont #-}

-- | Run a 'Cont' effect.
--
-- Compared to 'runCont', this is quite a bit faster, but is significantly more
-- restrictive in what interpreters are used after it, since there are very
-- few primitive effects that the carrier for 'runContFast' is able to thread.
-- In fact, of all the primitive effects featured in this library, only
-- one satisfies 'ContThreads': namely, 'Control.Effect.Reader.Reader'.
runContFast :: forall m a p
             . ( Carrier m
               , Threaders '[ContFastThreads] m p
               )
            => ContFastC a m a -> m a
runContFast = C.evalContT .# unContFastC
{-# INLINE runContFast #-}
