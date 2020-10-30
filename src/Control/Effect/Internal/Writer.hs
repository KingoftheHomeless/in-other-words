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
data Tell o :: Effect where
  Tell :: o -> Tell o m ()

-- | An effect for hearing what a computation
-- has to 'Control.Effect.Writer.tell'.
data Listen o :: Effect where
  Listen :: m a -> Listen o m (o, a)

-- | An effect for altering what a computation
-- 'Control.Effect.Writer.tell's.
newtype Pass o :: Effect where
  Pass :: m (o -> o, a) -> Pass o m a

newtype TellC o m a = TellC {
    unTellC :: WriterT o m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           )
       via WriterT o m
  deriving MonadTrans via (WriterT o)

instance MonadThrow m => MonadThrow (TellC o m) where
  throwM = lift . C.throwM
  {-# INLINEABLE throwM #-}

instance (Monoid o, MonadCatch m) => MonadCatch (TellC o m) where
  catch (TellC m) h = TellC $ writerT $
    runWriterT m `C.catch` (runWriterT . unTellC #. h)
  {-# INLINEABLE catch #-}

instance (Monoid o, MonadMask m) => MonadMask (TellC o m) where
  mask main = TellC $ writerT $ C.mask $ \restore ->
    runWriterT (unTellC (main (TellC #. W.mapWriterT restore .# unTellC)))
  {-# INLINEABLE mask #-}

  uninterruptibleMask main = TellC $ writerT $ C.uninterruptibleMask $ \restore ->
    runWriterT (unTellC (main (TellC #. W.mapWriterT restore .# unTellC)))
  {-# INLINEABLE uninterruptibleMask #-}

  generalBracket acquire release use =
    coerceAlg
      (threadEff @(WriterT o) @_ @m
        (\(GeneralBracket a r u) -> C.generalBracket a r u)
      )
      (GeneralBracket acquire release use)
  {-# INLINEABLE generalBracket #-}

instance MonadBase b m => MonadBase b (TellC o m) where
  liftBase = lift . liftBase
  {-# INLINEABLE liftBase #-}

instance ( MonadBaseControl b m
         , Monoid o
         )
        => MonadBaseControl b (TellC o m) where
  type StM (TellC o m) a = StM m (a, o)

  liftBaseWith = defaultLiftBaseWith
  {-# INLINEABLE liftBaseWith #-}

  restoreM = defaultRestoreM
  {-# INLINEABLE restoreM #-}

instance Monoid o => MonadTransControl (TellC o) where
  type StT (TellC o) a = (a, o)

  liftWith main = lift (main (runWriterT .# unTellC))
  {-# INLINEABLE liftWith #-}

  restoreT = TellC #. writerT
  {-# INLINEABLE restoreT #-}

instance ( Carrier m
         , Monoid o
         , Threads (WriterT o) (Prims m)
         )
      => Carrier (TellC o m) where
  type Derivs (TellC o m) = Tell o ': Derivs m
  type Prims  (TellC o m) = Prims m

  algPrims = coerceAlg (thread @(WriterT o) (algPrims @m))
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    Tell o -> n (TellC (W.tell o))
  {-# INLINEABLE reformulate #-}


newtype ListenC o m a = ListenC {
    unListenC :: WriterT o m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
       via TellC o m
  deriving (MonadTrans, MonadTransControl) via (TellC o)

instance ( Carrier m
         , Monoid o
         , Threads (WriterT o) (Prims m)
         )
      => Carrier (ListenC o m) where
  type Derivs (ListenC o m) = Listen o ': Tell o ': Derivs m
  type Prims  (ListenC o m) = ListenPrim o ': Prims m

  algPrims =
    powerAlg (
      coerce (algPrims @(TellC o m))
    ) $ \case
        ListenPrimTell o -> ListenC $ W.tell o
        ListenPrimListen (ListenC m) -> ListenC $ do
          (a, o) <- W.listen m
          return (o, a)
  {-# INLINEABLE algPrims #-}

  reformulate n alg =
    powerAlg (
      coerceReform (reformulate @(TellC o m)) n (weakenAlg alg)
    ) $ \case
      Listen m -> (alg . inj) $ ListenPrimListen m
  {-# INLINEABLE reformulate #-}


newtype WriterC o m a = WriterC {
    unWriterC :: WriterT o m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
       via TellC o m
  deriving (MonadTrans, MonadTransControl) via (TellC o)

instance ( Carrier m
         , Monoid o
         , Threads (WriterT o) (Prims m)
         )
      => Carrier (WriterC o m) where
  type Derivs (WriterC o m) = Pass o ': Listen o ': Tell o ': Derivs m
  type Prims  (WriterC o m) = WriterPrim o ': Prims m

  algPrims =
    algListenPrimIntoWriterPrim (
      coerce (algPrims @(ListenC o m))
    ) $ \(WriterC m) -> WriterC $ W.pass $ do
      (f, a) <- m
      return (a, f)
  {-# INLINEABLE algPrims #-}

  reformulate n alg =
    powerAlg (
    powerAlg (
      coerceReform (reformulate @(TellC o m)) n (weakenAlg alg)
    ) $ \case
      Listen m -> (alg . inj) $ WriterPrimListen m
    ) $ \case
      Pass m -> (alg . inj) $ WriterPrimPass m
  {-# INLINEABLE reformulate #-}


newtype TellLazyC o m a = TellLazyC {
    unTellLazyC :: LW.WriterT o m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadThrow, MonadCatch, MonadMask
           , MonadFix, MonadFail, MonadIO
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl)

instance ( Monoid o
         , Carrier m
         , Threads (LW.WriterT o) (Prims m)
         )
      => Carrier (TellLazyC o m) where
  type Derivs (TellLazyC o m) = Tell o ': Derivs m
  type Prims  (TellLazyC o m) = Prims m

  algPrims = coerce (thread @(LW.WriterT o) (algPrims @m))
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    Tell o -> n $ TellLazyC $ LW.tell o
  {-# INLINEABLE reformulate #-}

newtype ListenLazyC o m a = ListenLazyC {
    unListenLazyC :: LW.WriterT o m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadThrow, MonadCatch, MonadMask
           , MonadFix, MonadFail, MonadIO
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl)

instance ( Monoid o
         , Carrier m
         , Threads (LW.WriterT o) (Prims m)
         )
      => Carrier (ListenLazyC o m) where
  type Derivs (ListenLazyC o m) = Listen o ': Tell o ': Derivs m
  type Prims  (ListenLazyC o m) = ListenPrim o ': Prims m

  algPrims =
    powerAlg (
      coerce (algPrims @(TellLazyC o m))
    ) $ \case
      ListenPrimTell o ->
        ListenLazyC $ LW.tell o
      ListenPrimListen (ListenLazyC m) ->
        ListenLazyC $ swap <$> LW.listen m
  {-# INLINEABLE algPrims #-}

  reformulate n alg =
    powerAlg (
      coerceReform (reformulate @(TellLazyC o m)) n (weakenAlg alg)
    ) $ \case
      Listen m -> (alg . inj) $ ListenPrimListen m
  {-# INLINEABLE reformulate #-}

newtype WriterLazyC o m a = WriterLazyC {
    _unWriterLazyC :: LW.WriterT o m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadThrow, MonadCatch, MonadMask
           , MonadFix, MonadFail, MonadIO
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl)

instance ( Monoid o
         , Carrier m
         , Threads (LW.WriterT o) (Prims m)
         )
      => Carrier (WriterLazyC o m) where
  type Derivs (WriterLazyC o m) = Pass o ': Listen o ': Tell o ': Derivs m
  type Prims  (WriterLazyC o m) = WriterPrim o ': Prims m

  algPrims =
    algListenPrimIntoWriterPrim (
      coerce (algPrims @(ListenLazyC o m))
    ) $ \(WriterLazyC m) -> WriterLazyC $ LW.pass (swap <$> m)
  {-# INLINEABLE algPrims #-}

  reformulate n alg =
    powerAlg (
    powerAlg (
      coerceReform (reformulate @(TellLazyC o m)) n (weakenAlg alg)
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
-- * 'Control.Effect.Type.ListenPrim.ListenPrim' @o@ (when @o@ is a 'Monoid')
-- * 'Control.Effect.Type.WriterPrim.WriterPrim' @o@ (when @o@ is a 'Monoid')
-- * 'Control.Effect.Type.ReaderPrim.ReaderPrim' @i@
-- * 'Control.Effect.Mask.Mask'
-- * 'Control.Effect.Bracket.Bracket'
-- * 'Control.Effect.Fix.Fix'
-- * 'Control.Effect.NonDet.Split'
class    ( forall o. Monoid o => Threads (WriterT o) p
         ) => WriterThreads p
instance ( forall o. Monoid o => Threads (WriterT o) p
         ) => WriterThreads p

-- | 'WriterLazyThreads' accepts the following primitive effects:
--
-- * 'Control.Effect.Regional.Regional' @s@
-- * 'Control.Effect.Optional.Optional' @s@ (when @s@ is a functor)
-- * 'Control.Effect.BaseControl.BaseControl' @b@
-- * 'Control.Effect.Type.ListenPrim.ListenPrim' @o@ (when @o@ is a 'Monoid')
-- * 'Control.Effect.Type.WriterPrim.WriterPrim' @o@ (when @o@ is a 'Monoid')
-- * 'Control.Effect.Type.ReaderPrim.ReaderPrim' @i@
-- * 'Control.Effect.Mask.Mask'
-- * 'Control.Effect.Bracket.Bracket'
-- * 'Control.Effect.Fix.Fix'
-- * 'Control.Effect.NonDet.Split'
class    ( forall o. Monoid o => Threads (LW.WriterT o) p
         ) => WriterLazyThreads p
instance ( forall o. Monoid o => Threads (LW.WriterT o) p
         ) => WriterLazyThreads p
