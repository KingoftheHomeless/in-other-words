{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Writer where

import Data.Coerce
import Data.Tuple (swap)

import Control.Applicative
import Control.Monad

import Control.Effect
import Control.Effect.Bracket
import Control.Effect.Type.Tell
import Control.Effect.Type.Listen
import Control.Effect.Type.Pass

import Control.Effect.Carrier

import Control.Monad.Trans.Control hiding (embed)
import qualified Control.Monad.Catch as C

import Control.Monad.Trans.Writer.CPS (WriterT, writerT, runWriterT)
import qualified Control.Monad.Trans.Writer.CPS as W
import qualified Control.Monad.Trans.Writer.Lazy as LW

import Control.Effect.Internal.Utils

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
  {-# INLINE throwM #-}

instance (Monoid s, MonadCatch m) => MonadCatch (TellC s m) where
  catch (TellC m) h = TellC $ writerT $
    runWriterT m `C.catch` (runWriterT . unTellC #. h)
  {-# INLINE catch #-}

instance (Monoid s, MonadMask m) => MonadMask (TellC s m) where
  mask main = TellC $ writerT $ C.mask $ \restore ->
    runWriterT (unTellC (main (TellC #. W.mapWriterT restore .# unTellC)))
  {-# INLINE mask #-}

  uninterruptibleMask main = TellC $ writerT $ C.uninterruptibleMask $ \restore ->
    runWriterT (unTellC (main (TellC #. W.mapWriterT restore .# unTellC)))
  {-# INLINE uninterruptibleMask #-}

  generalBracket acquire release use =
    coerceAlg
      (threadEff @_ @(WriterT s) @m
        (\(GeneralBracket a r u) -> C.generalBracket a r u)
      )
      (GeneralBracket acquire release use)
  {-# INLINE generalBracket #-}

instance MonadBase b m => MonadBase b (TellC s m) where
  liftBase = lift . liftBase
  {-# INLINE liftBase #-}

instance ( MonadBaseControl b m
         , Monoid s
         )
        => MonadBaseControl b (TellC s m) where
  type StM (TellC s m) a = StM m (a, s)

  liftBaseWith = defaultLiftBaseWith
  {-# INLINE liftBaseWith #-}

  restoreM = defaultRestoreM
  {-# INLINE restoreM #-}

instance Monoid s => MonadTransControl (TellC s) where
  type StT (TellC s) a = (a, s)

  liftWith main = lift (main (runWriterT .# unTellC))
  {-# INLINE liftWith #-}

  restoreT = TellC #. writerT
  {-# INLINE restoreT #-}

instance ( Carrier m
         , Monoid s
         , Threads (WriterT s) (Prims m)
         )
      => Carrier (TellC s m) where
  type Derivs (TellC s m) = Tell s ': Derivs m
  type Prims  (TellC s m) = Prims m

  algPrims = coerceAlg (thread @(WriterT s) (algPrims @m))
  {-# INLINE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    Tell s -> n (TellC (W.tell s))
  {-# INLINE reformulate #-}


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
  type Prims  (ListenC s m) = Listen s ': Prims m

  algPrims =
    powerAlg (
      coerce (algPrims @(TellC s m))
    ) $ \case
        Listen (ListenC m) -> ListenC $ do
          (a, s) <- W.listen m
          return (s, a)
  {-# INLINE algPrims #-}

  reformulate = addPrim (coerceReform (reformulate @(TellC s m)))
  {-# INLINE reformulate #-}


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
  type Prims  (WriterC s m) = Pass s ': Listen s ': Prims m

  algPrims =
    powerAlg (
      coerce (algPrims @(ListenC s m))
    ) $ \case
        Pass (WriterC m) -> WriterC $ W.pass $ do
          (f, a) <- m
          return (a, f)
  {-# INLINE algPrims #-}

  reformulate = addPrim (coerceReform (reformulate @(ListenC s m)))
  {-# INLINE reformulate #-}


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
  {-# INLINE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    Tell s -> n $ TellLazyC $ LW.tell s
  {-# INLINE reformulate #-}

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
  type Prims  (ListenLazyC s m) = Listen s ': Prims m

  algPrims =
    powerAlg (
      coerce (algPrims @(TellLazyC s m))
    ) $ \case
      Listen (ListenLazyC m) -> ListenLazyC $ swap <$> LW.listen m
  {-# INLINE algPrims #-}

  reformulate = addPrim (coerceReform (reformulate @(TellLazyC s m)))
  {-# INLINE reformulate #-}

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
  type Prims  (WriterLazyC s m) = Pass s ': Listen s ': Prims m

  algPrims =
    powerAlg (
      coerce (algPrims @(ListenLazyC s m))
    ) $ \case
      Pass (WriterLazyC m) -> WriterLazyC $ LW.pass (swap <$> m)
  {-# INLINE algPrims #-}

  reformulate = addPrim (coerceReform (reformulate @(ListenLazyC s m)))
  {-# INLINE reformulate #-}

class    ( forall s. Monoid s => Threads (WriterT s) p
         ) => WriterThreads p
instance ( forall s. Monoid s => Threads (WriterT s) p
         ) => WriterThreads p

class    ( forall s. Monoid s => Threads (LW.WriterT s) p
         ) => WriterLazyThreads p
instance ( forall s. Monoid s => Threads (LW.WriterT s) p
         ) => WriterLazyThreads p
