{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Cont where

import Data.Coerce

import Control.Monad.Trans
import Control.Monad.Base
import qualified Control.Monad.Fail as Fail

import Control.Effect
import Control.Effect.Carrier

import Control.Effect.Internal.Utils

import Control.Monad.Trans.Free.Church.Alternate

-- | An effect for abortive continuations.
newtype Cont :: Effect where
  CallCC :: ((forall b. a -> m b) -> m a) -> Cont m a

-- | An effect for non-abortive continuations of a program
-- that eventually produces a result of type @r@.
--
-- This isn't quite as powerful as proper delimited continuations,
-- as this doesn't provide any equivalent of the @reset@ operator.
--
-- This can be useful as a helper effect.
newtype Shift r :: Effect where
  Shift :: ((a -> m r) -> m r) -> Shift r m a

data ContBase mr r a where
  Exit    :: r -> ContBase mr r void
  Attempt :: mr -> ContBase mr r r
  GetCont :: ContBase mr r (Either (a -> mr) a)


newtype ContC r m a = ContC { unContC :: FreeT (ContBase (m r) r) m a }
  deriving ( Functor, Applicative, Monad
           , MonadBase b, Fail.MonadFail, MonadIO
           , MonadThrow, MonadCatch
           )

instance MonadTrans (ContC s) where
  lift = ContC #. lift
  {-# INLINE lift #-}

instance ( Carrier m
         , Threads (FreeT (ContBase (m r) r)) (Prims m)
         )
      => Carrier (ContC r m) where
  type Derivs (ContC r m) = Cont ': Derivs m
  type Prims  (ContC r m) = Prims m

  algPrims = coerce (thread @(FreeT (ContBase (m r) r)) (algPrims @m))
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    CallCC main -> n (ContC $ liftF $ GetCont) >>= \case
      Left c  -> main (\x -> n $ ContC $ liftF (Attempt (c x)) >>= liftF . Exit)
      Right a -> return a
  {-# INLINEABLE reformulate #-}

newtype ShiftC r m a = ShiftC { unShiftC :: FreeT (ContBase (m r) r) m a }
  deriving ( Functor, Applicative, Monad
           , MonadBase b, Fail.MonadFail, MonadIO
           , MonadThrow, MonadCatch
           )

instance MonadTrans (ShiftC s) where
  lift = ShiftC #. lift
  {-# INLINE lift #-}

instance ( Carrier m
         , Threads (FreeT (ContBase (m r) r)) (Prims m)
         )
      => Carrier (ShiftC r m) where
  type Derivs (ShiftC r m) = Shift r ': Derivs m
  type Prims  (ShiftC r m) = Prims m

  algPrims = coerce (thread @(FreeT (ContBase (m r) r)) (algPrims @m))
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    Shift main -> n (ShiftC $ liftF $ GetCont) >>= \case
      Left c  -> main (\x -> n $ ShiftC $ liftF $ Attempt (c x)) >>= \r ->
        n (ShiftC $ liftF $ Exit r)
      Right a -> return a
  {-# INLINEABLE reformulate #-}

-- | 'ContThreads' accepts the following primitive effects:
--
-- * 'Control.Effect.Regional.Regional' @s@
-- * 'Control.Effect.Optional.Optional' @s@ (when @s@ is a functor)
-- * 'Control.Effect.Type.Unravel.Unravel' @p@
-- * 'Control.Effect.Type.ListenPrim.ListenPrim' @o@ (when @o@ is a 'Monoid')
-- * 'Control.Effect.Type.ReaderPrim.ReaderPrim' @i@
type ContThreads = FreeThreads
