{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Cont where

import Data.Coerce

import Control.Monad.Trans
import Control.Monad.Base
import qualified Control.Monad.Fail as Fail

import Control.Effect
import Control.Effect.Carrier

import Control.Effect.Internal.Utils

import qualified Control.Monad.Trans.Cont as C
import Control.Monad.Trans.Free.Church.Alternate

-- | An effect for abortive continuations.
newtype Cont m a where
  CallCC :: ((forall b. a -> m b) -> m a) -> Cont m a

-- | An effect for non-abortive continuations of a program
-- that eventually produces a result of type @r@.
--
-- This isn't quite as powerful as proper delimited continuations,
-- as this doesn't provide any equivalent of the @reset@ operator.
--
-- This can be useful as a helper effect.
newtype Shift r m a where
  Shift :: ((a -> m r) -> m r) -> Shift r m a

data ContBase r a where
  Exit    :: r -> ContBase r a
  GetCont :: ContBase r (Either (a -> r) a)


newtype ContC r m a = ContC { unContC :: FreeT (ContBase (m r)) m a }
  deriving ( Functor, Applicative, Monad
           , MonadBase b, Fail.MonadFail, MonadIO
           , MonadThrow, MonadCatch
           )

instance MonadTrans (ContC s) where
  lift = ContC #. lift
  {-# INLINE lift #-}

instance ( Carrier m
         , Threads (FreeT (ContBase (m r))) (Prims m)
         )
      => Carrier (ContC r m) where
  type Derivs (ContC r m) = Cont ': Derivs m
  type Prims  (ContC r m) = Prims m

  algPrims = coerce (thread @(FreeT (ContBase (m r))) (algPrims @m))
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    CallCC main -> n (ContC $ liftF $ GetCont) >>= \case
      Left c  -> main (n . ContC #. liftF . Exit . c)
      Right a -> return a
  {-# INLINEABLE reformulate #-}


newtype ContFastC (r :: *) m a = ContFastC { unContFastC :: C.ContT r m a }
  deriving (Functor, Applicative, Monad, MonadBase b, MonadIO, Fail.MonadFail)
  deriving MonadTrans

instance ( Carrier m
         , Threads (C.ContT r) (Prims m)
         )
      => Carrier (ContFastC r m) where
  type Derivs (ContFastC r m) = Cont ': Derivs m
  type Prims  (ContFastC r m) = Prims m

  algPrims = coerce (thread @(C.ContT r) (algPrims @m))
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    CallCC main ->
      n (ContFastC $ C.ContT $ \c -> c (Left (c . Right))) >>= \case
        Left c  -> main (\a -> n $ ContFastC $ C.ContT $ \_ -> c a)
        Right a -> return a
  {-# INLINEABLE reformulate #-}

newtype ShiftC r m a = ShiftC { unShiftC :: FreeT (ContBase (m r)) m a }
  deriving ( Functor, Applicative, Monad
           , MonadBase b, Fail.MonadFail, MonadIO
           , MonadThrow, MonadCatch
           )

instance MonadTrans (ShiftC s) where
  lift = ShiftC #. lift
  {-# INLINE lift #-}

instance ( Carrier m
         , Threads (FreeT (ContBase (m r))) (Prims m)
         )
      => Carrier (ShiftC r m) where
  type Derivs (ShiftC r m) = Shift r ': Derivs m
  type Prims  (ShiftC r m) = Prims m

  algPrims = coerce (thread @(FreeT (ContBase (m r))) (algPrims @m))
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    Shift main -> n (ShiftC $ liftF $ GetCont) >>= \case
      Left c  -> main (n . lift . c) >>= \r ->
        n (ShiftC $ liftF $ Exit (pure r))
      Right a -> return a
  {-# INLINEABLE reformulate #-}

instance ( Carrier m
         , Threads (C.ContT r) (Prims m)
         )
      => Carrier (ShiftFastC r m) where
  type Derivs (ShiftFastC r m) = Shift r ': Derivs m
  type Prims  (ShiftFastC r m) = Prims m

  algPrims = coerce (thread @(C.ContT r) (algPrims @m))
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    Shift main ->
      n (ShiftFastC $ C.ContT $ \c -> c (Left (c . Right))) >>= \case
        Left c  -> main (n . lift . c) >>= \r ->
          n (ShiftFastC $ C.ContT $ \_ -> return r)
        Right a -> return a
  {-# INLINEABLE reformulate #-}

newtype ShiftFastC (r :: *) m a = ShiftFastC { unShiftFastC :: C.ContT r m a }
  deriving (Functor, Applicative, Monad, MonadBase b, MonadIO, Fail.MonadFail)
  deriving MonadTrans

-- | 'ContThreads' accepts the following primitive effects:
--
-- * 'Control.Effect.Regional.Regional' @s@
-- * 'Control.Effect.Optional.Optional' @s@ (when @s@ is a functor)
-- * 'Control.Effect.Type.Unravel.Unravel' @p@
-- * 'Control.Effect.Type.ListenPrim.ListenPrim' @s@ (when @s@ is a 'Monoid')
-- * 'Control.Effect.Type.ReaderPrim.ReaderPrim' @i@
type ContThreads = FreeThreads

-- | 'ContFastThreads' accepts the following primitive effects:
--
-- * 'Control.Effect.Type.ReaderPrim.ReaderPrim' @i@
class    ( forall s. Threads (C.ContT s) p
         ) => ContFastThreads p
instance ( forall s. Threads (C.ContT s) p
         ) => ContFastThreads p
