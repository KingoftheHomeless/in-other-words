module Control.Effect.Cont
  ( -- * Effects
    Cont(..)
  , Shift(..)

    -- * Actions
  , callCC
  , shift

    -- * Interpretations
  , runCont
  , runContFast

  , runShift
  , runShiftFast

  , contToShift

    -- * Threading constraints
  , ContThreads
  , ContFastThreads

    -- * Carriers
  , ContC
  , ContFastC
  , ShiftC
  , ShiftFastC
  , ContToShiftC
  ) where

import Data.Coerce

import Control.Effect
import Control.Effect.Internal.Cont

import Control.Effect.Internal.Utils

import qualified Control.Monad.Trans.Cont as C
import Control.Monad.Trans.Free.Church.Alternate

-- | Call with current continuation. The argument computation is provided
-- the /continuation/ of the program at the point that 'callCC' was invoked.
-- If the continuation is executed, then control will immediately abort
-- and jump to the point 'callCC' was invoked, which will then return
-- the argument provided to the continuation.
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

-- | Non-abortive call with current continuation. The argument computation is
-- provided the /continuation/ of the program at the point that 'shift' was invoked.
-- If the continuation is executed, then control will jump to the point 'shift'
-- was invoked, which will then return the argument provided to the continuation.
--
-- Once the program finishes, and produces an @r@, control will jump /back/
-- to where the continuation was executed, and return that @r@.
-- From that point, you may decide whether or not to modify the final @r@,
-- or invoke the continuation again with a different argument.
--
-- You can also use 'shift' to abort the execution of the program early
-- by simply not executing the provided continuation, and instead
-- provide the final @r@ directly.
--
-- The way higher-order actions interact with the continuation depends
-- on the interpretation of 'Shift'. In general, you cannot expect to interact
-- with the continuation in any meaningful way: for example, you should not
-- assume that you will be able to catch an exception thrown at some point in
-- the future of the computation by using 'Control.Effect.Error.catch' on the
-- continuation.
shift :: Eff (Shift r) m
      => ((a -> m r) -> m r) -> m a
shift = send .# Shift
{-# INLINE shift #-}

-- | Run a 'Cont' effect.
--
-- @'Derivs' ('ContC' r m) = 'Cont' ': 'Derivs' m@
--
-- @'Control.Effect.Primitive.Prims'  ('ContC' r m) = 'Prims' m@
runCont :: forall a m p
         . ( Carrier m
           , Threaders '[ContThreads] m p
           )
        => ContC a m a -> m a
runCont =
    foldFreeT
      id
      (\c -> \case
        Exit a -> a
        GetCont -> c $ Left (c . Right)
      )
  .# unContC
{-# INLINE runCont #-}

-- | Run a 'Cont' effect.
--
-- Compared to 'runCont', this is quite a bit faster, but is significantly more
-- restrictive in what interpreters are used after it, since there are very
-- few primitive effects that the carrier for 'runContFast' is able to thread.
-- In fact, of all the primitive effects provided by this library, only
-- one satisfies 'ContFastThreads': namely,
-- 'Control.Effect.Type.ReaderPrim.ReaderPrim'.
--
-- @'Derivs' ('ContFastC' r m) = 'Cont' ': 'Derivs' m@
--
-- @'Control.Effect.Primitive.Prims'  ('ContFastC' r m) = 'Control.Effect.Primitive.Prims' m@
runContFast :: forall a m p
             . ( Carrier m
               , Threaders '[ContFastThreads] m p
               )
            => ContFastC a m a -> m a
runContFast = C.evalContT .# unContFastC
{-# INLINE runContFast #-}

-- | Run a @'Shift' r@ effect if the program returns @r@.
--
-- @'Derivs' ('ShiftC' r m) = 'Shift' r ': 'Derivs' m@
--
-- @'Control.Effect.Primitive.Prims'  ('ShiftC' r m) = 'Control.Effect.Primitive.Prims' m@
runShift :: forall r m p
          . ( Carrier m
            , Threaders '[ContThreads] m p
            )
         => ShiftC r m r -> m r
runShift = coerce (runCont @r @m @p)
{-# INLINE runShift #-}

-- | Run a @'Shift' r@ effect if the program returns @r@.
--
-- Compared to 'runCont', this is quite a bit faster, but is significantly more
-- restrictive in what interpreters are used after it, since there are very
-- few primitive effects that the carrier for 'runContFast' is able to thread.
-- In fact, of all the primitive effects provided by this library, only
-- one satisfies 'ContFastThreads': namely,
-- 'Control.Effect.Type.ReaderPrim.ReaderPrim'.
--
-- @'Derivs' ('ShiftFastC' r m) = 'Cont' ': 'Derivs' m@
--
-- @'Control.Effect.Primitive.Prims'  ('ShiftFastC' r m) = 'Control.Effect.Primitive.Prims' m@
runShiftFast :: forall r m p
              . ( Carrier m
                , Threaders '[ContFastThreads] m p
                )
             => ShiftFastC r m r -> m r
runShiftFast = C.evalContT .# unShiftFastC
{-# INLINE runShiftFast #-}

data ContToShiftH r

instance Eff (Shift r) m
      => Handler (ContToShiftH r) Cont m where
  effHandler = \case
    CallCC main -> shift @r $ \c ->
      main (\a -> shift $ \_ -> c a) >>= c
  {-# INLINEABLE effHandler #-}

type ContToShiftC r = InterpretC (ContToShiftH r) Cont

-- | Transform a 'Cont' effect into a @'Shift' r@ effect.
contToShift :: Eff (Shift r) m
            => ContToShiftC r m a
            -> m a
contToShift = interpretViaHandler
{-# INLINE contToShift #-}
