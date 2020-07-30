{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Reader where

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

-- | An effect for obtaining information from an
-- environment and locally modifying that environment.
--
-- **'Reader' is typically used as a primitive effect.**
-- If you define a 'Control.Effect.Carrier' that relies on a novel
-- non-trivial monad transformer, then you need to make a
-- a @'ThreadsEff' ('Reader' i)@ instance for that monad transformer
-- (if possible). 'threadReaderViaClass' and 'threadReaderViaRegional'
-- can help you with that.
data Reader i m a where
  Ask   :: Reader i m i
  Local :: (i -> i) -> m a -> Reader i m a

instance ( Reifies s (ReifiedEffAlgebra (Reader i) m)
         , Monad m
         ) => MonadReader i (ViaAlg s (Reader i) m) where
  ask = case reflect @s of
    ReifiedEffAlgebra alg -> coerceAlg alg Ask
  {-# INLINE ask #-}

  local f m = case reflect @s of
    ReifiedEffAlgebra alg -> coerceAlg alg (Local f m)
  {-# INLINE local #-}

-- | A valid definition of 'threadEff' for a @'ThreadsEff' ('Reader' i) t@ instance,
-- given that @t@ lifts @'MonadReader' i@.
threadReaderViaClass :: forall i t m a
                      . Monad m
                     => ( RepresentationalT t
                        , MonadTrans t
                        , forall b. MonadReader i b => MonadReader i (t b)
                        )
                     => (forall x. Reader i m x -> m x)
                     -> Reader i (t m) a -> t m a
threadReaderViaClass alg e = reify (ReifiedEffAlgebra alg) $ \(_ :: pr s) ->
  case e of
    Ask -> lift (alg Ask)
    Local f m -> unViaAlgT (RC.local f (viaAlgT @s @(Reader i) m))
{-# INLINE threadReaderViaClass #-}

-- | A valid definition of 'threadEff' for a @'ThreadsEff' ('Reader' i) t@ instance,
-- given that @t@ threads @'Regional' s@ for any @s@.
threadReaderViaRegional :: forall i t m a
                         . ( Monad m
                           , MonadTrans t
                           , ThreadsEff (Regional ()) t
                           )
                        => (forall x. Reader i m x -> m x)
                        -> Reader i (t m) a -> t m a
threadReaderViaRegional alg Ask = lift (alg Ask)
threadReaderViaRegional alg (Local f m) =
  threadEff (\(Regional _ m') -> alg $ (Local f m')) (Regional () m)
{-# INLINE threadReaderViaRegional #-}

#define THREADREADER(monadT)                              \
instance ThreadsEff (Reader threadedInput) (monadT) where \
  threadEff = threadReaderViaClass;                       \
  {-# INLINE threadEff #-}

#define THREADREADER_CTX(ctx, monadT)                            \
instance ctx => ThreadsEff (Reader threadedInput) (monadT) where \
  threadEff = threadReaderViaClass;                              \
  {-# INLINE threadEff #-}

instance ThreadsEff (Reader i) (ReaderT i') where
  threadEff alg = \case
    Ask -> lift (alg Ask)
    Local f m -> R.mapReaderT (alg . Local f) m
  {-# INLINE threadEff #-}

instance Monoid w => ThreadsEff (Reader i) (CPSWr.WriterT w) where
  threadEff alg = \case
    Ask -> lift (alg Ask)
    Local f m -> CPSWr.mapWriterT (alg . Local f) m
  {-# INLINE threadEff #-}

-- TODO(KingoftheHomeless): Benchmark this vs hand-written instances.
THREADREADER(ExceptT e)
THREADREADER(SSt.StateT s)
THREADREADER(LSt.StateT s)
THREADREADER_CTX(Monoid w, LWr.WriterT w)
THREADREADER_CTX(Monoid w, SWr.WriterT w)
THREADREADER(C.ContT r)
