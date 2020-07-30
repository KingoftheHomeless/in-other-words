{-# LANGUAGE CPP, MagicHash #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Internal.BaseControl where

import Data.Coerce
import GHC.Exts (Proxy#, proxy#)
import Control.Effect.Internal.Union
import Control.Effect.Internal.Utils
import Control.Effect.Internal.Itself
import Control.Effect.Type.Optional
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict as SSt
import Control.Monad.Trans.State.Lazy as LSt
import Control.Monad.Trans.Writer.Lazy as LWr
import Control.Monad.Trans.Writer.Strict as SWr
import Control.Monad.Trans.Writer.CPS as CPSWr

-- | A /helper primitive effect/ that allows for lowering computations to a
-- base monad.
--
-- Helper primitive effects are effects that allow you to avoid interpreting one
-- of your own effects as a primitive if the power needed from direct access to
-- the underlying monad can instead be provided by the relevant helper primitive
-- effect. The reason why you'd want to do this is that helper primitive effects
-- already have 'ThreadsEff' instances defined for them; so you don't have to
-- define any for your own effect.
--
-- The helper primitive effects offered in this library are - in order
-- of ascending power - 'Regional', 'Optional', 'BaseControl' and 'Unlift'.
--
-- **'BaseControl' is typically used as a primitive effect**.
-- If you define a 'Control.Effect.Carrier' that relies on a novel
-- non-trivial monad transformer, then you need to make a
-- a @'ThreadsEff' ('BaseControl' b)@ instance for that monad transformer
-- (if possible). 'threadBaseControlViaClass' can help you with that.
newtype BaseControl b m a where
  GainBaseControl :: (  forall z
                      . (MonadBaseControl b z, Coercible z m)
                     => Proxy# z
                     -> a
                     )
                  -> BaseControl b m a

-- | A valid definition of 'threadEff' for a @'ThreadsEff' ('BaseControl' b) t@ instance,
-- given that @t@ lifts @'MonadBaseControl' b@ for any @b@.
threadBaseControlViaClass :: forall b t m a
                           . ( MonadTrans t
                             , Monad m
                             ,    forall z
                                . MonadBaseControl b z
                               => MonadBaseControl b (t z)
                             ,    forall z
                                . Coercible z m
                              => Coercible (t z) (t m)
                             )
                          => (forall x. BaseControl b m x -> m x)
                          -> BaseControl b (t m) a -> t m a
threadBaseControlViaClass alg (GainBaseControl main) =
  lift $ alg $ GainBaseControl $ \(_ :: Proxy# z) -> main (proxy# @_ @(t z))
{-# INLINE threadBaseControlViaClass #-}

-- | A valid definition of 'threadEff' for a @'ThreadsEff' ('Optional' s) t@ instance,
-- given that @t@ threads @'BaseControl' b@ for any @b@.
threadOptionalViaBaseControl :: forall s t m a
                              . ( Functor s
                                , Monad m
                                , Monad (t m)
                                , ThreadsEff (BaseControl m) t
                                )
                             => (forall x. Optional s m x -> m x)
                             -> Optional s (t m) a -> t m a
threadOptionalViaBaseControl alg (Optional sa m) =
    join
  $ threadEff (\(GainBaseControl main) -> return $ main (proxy# @_ @(Itself m)))
  $ GainBaseControl @m $ \(_ :: Proxy# z) ->
      coerce $ control @m @z @a $ \lower -> do
        join $ coerce
             $ alg
             $ Optional (fmap (lower . pure) sa)
                        (fmap pure (coerce (lower @a) m))
{-# INLINE threadOptionalViaBaseControl #-}


#define THREAD_BASE_CONTROL(monadT)                \
instance ThreadsEff (BaseControl b) (monadT) where \
  threadEff = threadBaseControlViaClass;           \
  {-# INLINE threadEff #-}

#define THREAD_BASE_CONTROL_CTX(ctx, monadT)              \
instance ctx => ThreadsEff (BaseControl b) (monadT) where \
  threadEff = threadBaseControlViaClass;                  \
  {-# INLINE threadEff #-}

THREAD_BASE_CONTROL(ReaderT i)
THREAD_BASE_CONTROL(ExceptT e)
THREAD_BASE_CONTROL(LSt.StateT s)
THREAD_BASE_CONTROL(SSt.StateT s)
THREAD_BASE_CONTROL_CTX(Monoid w, LWr.WriterT w)
THREAD_BASE_CONTROL_CTX(Monoid w, SWr.WriterT w)

-- monad-control still doesn't have a MonadBaseControl instance for CPS
-- WriterT, so we use a work-around to make this instance.
instance Monoid w => ThreadsEff (BaseControl b) (CPSWr.WriterT w) where
  threadEff alg (GainBaseControl main) =
    lift $ alg $ GainBaseControl $ \(_ :: Proxy# z) ->
      main (proxy# @_ @(WriterCPS w z))
  {-# INLINE threadEff #-}


newtype WriterCPS s m a = WriterCPS { unWriterCPS :: CPSWr.WriterT s m a }
  deriving (Functor, Applicative, Monad)
  deriving MonadTrans

instance MonadBase b m => MonadBase b (WriterCPS s m) where
  liftBase = lift . liftBase
  {-# INLINE liftBase #-}

instance (Monoid s, MonadBaseControl b m)
      => MonadBaseControl b (WriterCPS s m) where
  type StM (WriterCPS s m) a = StM m (a, s)

  liftBaseWith main = lift $ liftBaseWith $ \run_it ->
    main (run_it . CPSWr.runWriterT .# unWriterCPS)
  {-# INLINE liftBaseWith #-}

  restoreM = WriterCPS #. CPSWr.writerT . restoreM
  {-# INLINE restoreM #-}
