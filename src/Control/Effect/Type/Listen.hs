{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Listen where

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.State.Strict as SSt
import qualified Control.Monad.Trans.State.Lazy as LSt
import qualified Control.Monad.Trans.Writer.Lazy as LWr
import qualified Control.Monad.Trans.Writer.Strict as SWr
import qualified Control.Monad.Trans.Writer.CPS as CPSWr
import Control.Monad.Writer.Class
import Control.Effect.Internal.ViaAlg
import Control.Effect.Internal.Reflection
import Control.Effect.Internal.Union

-- | An effect for hearing what a computation
-- has to 'Control.Effect.Writer.tell'.
--
-- **'Listen' is typically used as a primitive effect.**
-- If you define a 'Control.Effect.Carrier' that relies on a novel
-- non-trivial monad transformer, then you need to make a
-- a @'Monoid' s => 'ThreadsEff' ('Listen' s)@ instance for that monad
-- transformer (if possible). 'threadListenViaClass' can help you with that.
data Listen s m a where
  Listen :: m a -> Listen s m (s, a)

instance ( Reifies s (ReifiedEffAlgebra (Listen w) m)
         , Monoid w
         , Monad m
         )
      => MonadWriter w (ViaAlg s (Listen w) m) where
  tell = error "threadListenViaClass: Transformers threading Listen \
                \are not allowed to use tell."

  pass = error "threadListenViaClass: Transformers threading Listen \
                 \are not allowed to use pass."

  listen m = case reflect @s of
    ReifiedEffAlgebra alg ->
      fmap (\(s, a) -> (a, s)) $ coerceAlg alg (Listen m)
  {-# INLINE listen #-}

-- | A valid definition of 'threadEff' for a @'ThreadsEff' ('Pass' w) t@ instance,
-- given that @t@ lifts @'MonadWriter' w@.
--
-- **BEWARE**: 'threadListenViaClass' is only safe if the implementation of
-- 'listen' for @t m@ only makes use of 'listen' for @m@, and no other methods
-- of 'MonadWriter'.
threadListenViaClass :: forall w t m a
                      . (Monoid w, Monad m)
                     => ( RepresentationalT t
                        , forall b. MonadWriter w b => MonadWriter w (t b)
                        )
                     => (forall x. Listen w m x -> m x)
                     -> Listen w (t m) a -> t m a
threadListenViaClass alg (Listen m) =
  reify (ReifiedEffAlgebra alg) $ \(_ :: pr s) ->
      unViaAlgT
    $ fmap (\(a, s) -> (s, a))
    $ listen
    $ viaAlgT @s @(Listen w) m
{-# INLINE threadListenViaClass #-}

#define THREAD_LISTEN(monadT)                              \
instance Monoid threadedMonoid                             \
      => ThreadsEff (Listen threadedMonoid) (monadT) where \
  threadEff = threadListenViaClass;                        \
  {-# INLINE threadEff #-}

THREAD_LISTEN(ReaderT i)
THREAD_LISTEN(ExceptT e)
THREAD_LISTEN(LSt.StateT s)
THREAD_LISTEN(SSt.StateT s)

instance ThreadsEff (Listen w) (LWr.WriterT s) where
  threadEff alg (Listen m) =
      LWr.WriterT
    $ fmap (\(s, (a, w)) -> ((s, a), w))
    $ alg
    $ Listen
    $ LWr.runWriterT m
  {-# INLINE threadEff #-}

instance ThreadsEff (Listen w) (SWr.WriterT s) where
  threadEff alg (Listen m) =
      SWr.WriterT
    $ fmap (\(s, (a, w)) -> ((s, a), w))
    $ alg
    $ Listen
    $ SWr.runWriterT m
  {-# INLINE threadEff #-}

instance Monoid s => ThreadsEff (Listen w) (CPSWr.WriterT s) where
  threadEff alg (Listen m) =
      CPSWr.writerT
    $ fmap (\(s, (a, w)) -> ((s, a), w))
    $ alg
    $ Listen
    $ CPSWr.runWriterT m
  {-# INLINE threadEff #-}
