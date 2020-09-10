{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Writer where

import Data.Coerce
import Data.Tuple (swap)

import Control.Applicative
import Control.Monad

import Control.Effect
import Control.Effect.Bracket
import Control.Effect.Type.ListenPrim
import Control.Effect.Type.WriterPrim

import Control.Effect.Carrier

import Control.Monad.Trans.Control hiding (embed)
import qualified Control.Monad.Catch as C

import Control.Monad.Trans.Writer.CPS (WriterT, writerT, runWriterT)
import qualified Control.Monad.Trans.Writer.CPS as W
import qualified Control.Monad.Trans.Writer.Lazy as LW

import Control.Effect.Internal.Utils

-- | An effect for arbitrary output.
data Tell s m a where
  Tell :: s -> Tell s m ()

-- | An effect for hearing what a computation
-- has to 'Control.Effect.Writer.tell'.
data Listen s m a where
  Listen :: m a -> Listen s m (s, a)

-- | An effect for altering what a computation
-- 'Control.Effect.Writer.tell's.
data Pass s m a where
  Pass :: m (s -> s, a) -> Pass s m a

newtype TellC s m a = TellC {
    unTellC :: WriterT s m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           )
       via WriterT s m
  deriving MonadTrans via (WriterT s)

instance MonadThrow m => MonadThrow (TellC s m) where
  throwM = lift . C.throwM
  {-# INLINEABLE throwM #-}

instance (Monoid s, MonadCatch m) => MonadCatch (TellC s m) where
  catch (TellC m) h = TellC $ writerT $
    runWriterT m `C.catch` (runWriterT . unTellC #. h)
  {-# INLINEABLE catch #-}

instance (Monoid s, MonadMask m) => MonadMask (TellC s m) where
  mask main = TellC $ writerT $ C.mask $ \restore ->
    runWriterT (unTellC (main (TellC #. W.mapWriterT restore .# unTellC)))
  {-# INLINEABLE mask #-}

  uninterruptibleMask main = TellC $ writerT $ C.uninterruptibleMask $ \restore ->
    runWriterT (unTellC (main (TellC #. W.mapWriterT restore .# unTellC)))
  {-# INLINEABLE uninterruptibleMask #-}

  generalBracket acquire release use =
    coerceAlg
      (threadEff @(WriterT s) @_ @m
        (\(GeneralBracket a r u) -> C.generalBracket a r u)
      )
      (GeneralBracket acquire release use)
  {-# INLINEABLE generalBracket #-}

instance MonadBase b m => MonadBase b (TellC s m) where
  liftBase = lift . liftBase
  {-# INLINEABLE liftBase #-}

instance ( MonadBaseControl b m
         , Monoid s
         )
        => MonadBaseControl b (TellC s m) where
  type StM (TellC s m) a = StM m (a, s)

  liftBaseWith = defaultLiftBaseWith
  {-# INLINEABLE liftBaseWith #-}

  restoreM = defaultRestoreM
  {-# INLINEABLE restoreM #-}

instance Monoid s => MonadTransControl (TellC s) where
  type StT (TellC s) a = (a, s)

  liftWith main = lift (main (runWriterT .# unTellC))
  {-# INLINEABLE liftWith #-}

  restoreT = TellC #. writerT
  {-# INLINEABLE restoreT #-}

instance ( Carrier m
         , Monoid s
         , Threads (WriterT s) (Prims m)
         )
      => Carrier (TellC s m) where
  type Derivs (TellC s m) = Tell s ': Derivs m
  type Prims  (TellC s m) = Prims m

  algPrims = coerceAlg (thread @(WriterT s) (algPrims @m))
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    Tell s -> n (TellC (W.tell s))
  {-# INLINEABLE reformulate #-}


newtype ListenC s m a = ListenC {
    unListenC :: WriterT s m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
       via TellC s m
  deriving (MonadTrans, MonadTransControl) via (TellC s)

instance ( Carrier m
         , Monoid s
         , Threads (WriterT s) (Prims m)
         )
      => Carrier (ListenC s m) where
  type Derivs (ListenC s m) = Listen s ': Tell s ': Derivs m
  type Prims  (ListenC s m) = ListenPrim s ': Prims m

  algPrims =
    powerAlg (
      coerce (algPrims @(TellC s m))
    ) $ \case
        ListenPrimTell s -> ListenC $ W.tell s
        ListenPrimListen (ListenC m) -> ListenC $ do
          (a, s) <- W.listen m
          return (s, a)
  {-# INLINEABLE algPrims #-}

  reformulate n alg =
    powerAlg (
      coerceReform (reformulate @(TellC s m)) n (weakenAlg alg)
    ) $ \case
      Listen m -> (alg . inj) $ ListenPrimListen m
  {-# INLINEABLE reformulate #-}


newtype WriterC s m a = WriterC {
    unWriterC :: WriterT s m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
       via TellC s m
  deriving (MonadTrans, MonadTransControl) via (TellC s)

instance ( Carrier m
         , Monoid s
         , Threads (WriterT s) (Prims m)
         )
      => Carrier (WriterC s m) where
  type Derivs (WriterC s m) = Pass s ': Listen s ': Tell s ': Derivs m
  type Prims  (WriterC s m) = WriterPrim s ': Prims m

  algPrims =
    algListenPrimIntoWriterPrim (
      coerce (algPrims @(ListenC s m))
    ) $ \(WriterC m) -> WriterC $ W.pass $ do
      (f, a) <- m
      return (a, f)
  {-# INLINEABLE algPrims #-}

  reformulate n alg =
    powerAlg (
    powerAlg (
      coerceReform (reformulate @(TellC s m)) n (weakenAlg alg)
    ) $ \case
      Listen m -> (alg . inj) $ WriterPrimListen m
    ) $ \case
      Pass m -> (alg . inj) $ WriterPrimPass m
  {-# INLINEABLE reformulate #-}


newtype TellLazyC s m a = TellLazyC {
    unTellLazyC :: LW.WriterT s m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadThrow, MonadCatch, MonadMask
           , MonadFix, MonadFail, MonadIO
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl)

instance ( Monoid s
         , Carrier m
         , Threads (LW.WriterT s) (Prims m)
         )
      => Carrier (TellLazyC s m) where
  type Derivs (TellLazyC s m) = Tell s ': Derivs m
  type Prims  (TellLazyC s m) = Prims m

  algPrims = coerce (thread @(LW.WriterT s) (algPrims @m))
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    Tell s -> n $ TellLazyC $ LW.tell s
  {-# INLINEABLE reformulate #-}

newtype ListenLazyC s m a = ListenLazyC {
    unListenLazyC :: LW.WriterT s m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadThrow, MonadCatch, MonadMask
           , MonadFix, MonadFail, MonadIO
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl)

instance ( Monoid s
         , Carrier m
         , Threads (LW.WriterT s) (Prims m)
         )
      => Carrier (ListenLazyC s m) where
  type Derivs (ListenLazyC s m) = Listen s ': Tell s ': Derivs m
  type Prims  (ListenLazyC s m) = ListenPrim s ': Prims m

  algPrims =
    powerAlg (
      coerce (algPrims @(TellLazyC s m))
    ) $ \case
      ListenPrimTell w ->
        ListenLazyC $ LW.tell w
      ListenPrimListen (ListenLazyC m) ->
        ListenLazyC $ swap <$> LW.listen m
  {-# INLINEABLE algPrims #-}

  reformulate n alg =
    powerAlg (
      coerceReform (reformulate @(TellLazyC s m)) n (weakenAlg alg)
    ) $ \case
      Listen m -> (alg . inj) $ ListenPrimListen m
  {-# INLINEABLE reformulate #-}

newtype WriterLazyC s m a = WriterLazyC {
    _unWriterLazyC :: LW.WriterT s m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadThrow, MonadCatch, MonadMask
           , MonadFix, MonadFail, MonadIO
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl)

instance ( Monoid s
         , Carrier m
         , Threads (LW.WriterT s) (Prims m)
         )
      => Carrier (WriterLazyC s m) where
  type Derivs (WriterLazyC s m) = Pass s ': Listen s ': Tell s ': Derivs m
  type Prims  (WriterLazyC s m) = WriterPrim s ': Prims m

  algPrims =
    algListenPrimIntoWriterPrim (
      coerce (algPrims @(ListenLazyC s m))
    ) $ \(WriterLazyC m) -> WriterLazyC $ LW.pass (swap <$> m)
  {-# INLINEABLE algPrims #-}

  reformulate n alg =
    powerAlg (
    powerAlg (
      coerceReform (reformulate @(TellLazyC s m)) n (weakenAlg alg)
    ) $ \case
      Listen m -> (alg . inj) $ WriterPrimListen m
    ) $ \case
      Pass m -> (alg . inj) $ WriterPrimPass m
  {-# INLINEABLE reformulate #-}

-- | 'WriterThreads' accepts the following primitive effects:
--
-- * 'Control.Effect.Regional.Regional' @s@
-- * 'Control.Effect.Optional.Optional' @s@ (when @s@ is a functor)
-- * 'Control.Effect.BaseControl.BaseControl' @b@
-- * 'Control.Effect.Type.ListenPrim.ListenPrim' @s@ (when @s@ is a 'Monoid')
-- * 'Control.Effect.Type.WriterPrim.WriterPrim' @s@ (when @s@ is a 'Monoid')
-- * 'Control.Effect.Type.ReaderPrim.ReaderPrim' @i@
-- * 'Control.Effect.Mask.Mask'
-- * 'Control.Effect.Bracket.Bracket'
-- * 'Control.Effect.Fix.Fix'
-- * 'Control.Effect.NonDet.Split'
class    ( forall s. Monoid s => Threads (WriterT s) p
         ) => WriterThreads p
instance ( forall s. Monoid s => Threads (WriterT s) p
         ) => WriterThreads p

-- | 'WriterLazyThreads' accepts the following primitive effects:
--
-- * 'Control.Effect.Regional.Regional' @s@
-- * 'Control.Effect.Optional.Optional' @s@ (when @s@ is a functor)
-- * 'Control.Effect.BaseControl.BaseControl' @b@
-- * 'Control.Effect.Type.ListenPrim.ListenPrim' @s@ (when @s@ is a 'Monoid')
-- * 'Control.Effect.Type.WriterPrim.WriterPrim' @s@ (when @s@ is a 'Monoid')
-- * 'Control.Effect.Type.ReaderPrim.ReaderPrim' @i@
-- * 'Control.Effect.Mask.Mask'
-- * 'Control.Effect.Bracket.Bracket'
-- * 'Control.Effect.Fix.Fix'
-- * 'Control.Effect.NonDet.Split'
class    ( forall s. Monoid s => Threads (LW.WriterT s) p
         ) => WriterLazyThreads p
instance ( forall s. Monoid s => Threads (LW.WriterT s) p
         ) => WriterLazyThreads p
