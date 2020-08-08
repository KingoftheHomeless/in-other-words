{-# LANGUAGE CPP #-}
module Control.Effect.Type.ReaderPrim where

import Control.Monad.Trans

import Control.Monad.Reader.Class (MonadReader)
import qualified Control.Monad.Reader.Class as RC
import Control.Monad.Trans.Except (ExceptT(..))

import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as R

import qualified Control.Monad.Trans.State.Strict as SSt
import qualified Control.Monad.Trans.State.Lazy as LSt
import qualified Control.Monad.Trans.Writer.Lazy as LWr
import qualified Control.Monad.Trans.Writer.Strict as SWr
import qualified Control.Monad.Trans.Writer.CPS as CPSWr
import Control.Monad.Trans.Cont (ContT(..))
import qualified Control.Monad.Trans.Cont as C

import Control.Effect.Internal.ViaAlg
import Control.Effect.Type.Regional
import Control.Effect.Internal.Reflection
import Control.Effect.Internal.Union

-- | A primitive effect that may be used for
-- @'Control.Effect.Reader.Reader'@ interpreters.
--
-- This combines 'Control.Effect.Reader.Ask' and 'Control.Effect.Reader.Local',
-- which is relevant since certain monad transformers may only thread
-- 'Control.Effect.Reader.local' if they also have access to
-- 'Control.Effect.Reader.ask'.
--
-- __'ReaderPrim' is only used as a primitive effect.__
-- If you define a 'Control.Effect.Carrier' that relies on a novel
-- non-trivial monad transformer, then you need to make a
-- a @'ThreadsEff' ('ReaderPrim' i)@ instance for that monad transformer
-- (if possible). 'threadReaderPrimViaClass' and 'threadReaderPrimViaRegional'
-- can help you with that.
data ReaderPrim i m a where
  ReaderPrimAsk   :: ReaderPrim i m i
  ReaderPrimLocal :: (i -> i) -> m a -> ReaderPrim i m a

instance ( Reifies s (ReifiedEffAlgebra (ReaderPrim i) m)
         , Monad m
         ) => MonadReader i (ViaAlg s (ReaderPrim i) m) where
  ask = case reflect @s of
    ReifiedEffAlgebra alg -> coerceAlg alg ReaderPrimAsk
  {-# INLINE ask #-}

  local f m = case reflect @s of
    ReifiedEffAlgebra alg -> coerceAlg alg (ReaderPrimLocal f m)
  {-# INLINE local #-}

-- | A valid definition of 'threadEff' for a @'ThreadsEff' ('ReaderPrim' i) t@ instance,
-- given that @t@ lifts @'MonadReader' i@.
threadReaderPrimViaClass :: forall i t m a
                      . Monad m
                     => ( RepresentationalT t
                        , MonadTrans t
                        , forall b. MonadReader i b => MonadReader i (t b)
                        )
                     => (forall x. ReaderPrim i m x -> m x)
                     -> ReaderPrim i (t m) a -> t m a
threadReaderPrimViaClass alg e = reify (ReifiedEffAlgebra alg) $ \(_ :: pr s) ->
  case e of
    ReaderPrimAsk -> lift (alg ReaderPrimAsk)
    ReaderPrimLocal f m -> unViaAlgT (RC.local f (viaAlgT @s @(ReaderPrim i) m))
{-# INLINE threadReaderPrimViaClass #-}

-- | A valid definition of 'threadEff' for a @'ThreadsEff' ('ReaderPrim' i) t@ instance,
-- given that @t@ threads @'Regional' s@ for any @s@.
threadReaderPrimViaRegional :: forall i t m a
                         . ( Monad m
                           , MonadTrans t
                           , ThreadsEff (Regional ()) t
                           )
                        => (forall x. ReaderPrim i m x -> m x)
                        -> ReaderPrim i (t m) a -> t m a
threadReaderPrimViaRegional alg ReaderPrimAsk = lift (alg ReaderPrimAsk)
threadReaderPrimViaRegional alg (ReaderPrimLocal f m) =
  threadEff (\(Regionally _ m') -> alg $ (ReaderPrimLocal f m')) (Regionally () m)
{-# INLINE threadReaderPrimViaRegional #-}

#define THREAD_READER(monadT)                                 \
instance ThreadsEff (ReaderPrim threadedInput) (monadT) where \
  threadEff = threadReaderPrimViaClass;                       \
  {-# INLINE threadEff #-}

#define THREAD_READER_CTX(ctx, monadT)                               \
instance ctx => ThreadsEff (ReaderPrim threadedInput) (monadT) where \
  threadEff = threadReaderPrimViaClass;                              \
  {-# INLINE threadEff #-}

instance ThreadsEff (ReaderPrim i) (ReaderT i') where
  threadEff alg = \case
    ReaderPrimAsk -> lift (alg ReaderPrimAsk)
    ReaderPrimLocal f m -> R.mapReaderT (alg . ReaderPrimLocal f) m
  {-# INLINE threadEff #-}

instance Monoid w => ThreadsEff (ReaderPrim i) (CPSWr.WriterT w) where
  threadEff alg = \case
    ReaderPrimAsk -> lift (alg ReaderPrimAsk)
    ReaderPrimLocal f m -> CPSWr.mapWriterT (alg . ReaderPrimLocal f) m
  {-# INLINE threadEff #-}

-- TODO(KingoftheHomeless): Benchmark this vs hand-written instances.
THREAD_READER(ExceptT e)
THREAD_READER(SSt.StateT s)
THREAD_READER(LSt.StateT s)
THREAD_READER_CTX(Monoid w, LWr.WriterT w)
THREAD_READER_CTX(Monoid w, SWr.WriterT w)
THREAD_READER(C.ContT r)
