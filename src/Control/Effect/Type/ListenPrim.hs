{-# LANGUAGE CPP #-}
module Control.Effect.Type.ListenPrim
  ( -- * Effects
    ListenPrim(..)

    -- * Threading utilities
  , threadListenPrim
  , threadListenPrimViaClass
 ) where

import Control.Monad.Trans
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

-- | A primitive effect that may be used for
-- interpreters of connected 'Control.Effect.Writer.Tell' and
-- 'Control.Effect.Writer.Listen' effects.
--
-- This combines 'Control.Effect.Writer.Tell' and
-- 'Control.Effect.Writer.Listen'. This may be relevant if there
-- are monad transformers that may only lift
-- 'Control.Effect.Writer.listen' if they also have access to
-- 'Control.Effect.Writer.tell'.
--
-- __'ListenPrim' is only used as a primitive effect.__
-- If you define a 'Control.Effect.Carrier' that relies on a novel
-- non-trivial monad transformer @t@, then you need to make a
-- a @'Monoid' w => 'ThreadsEff' t ('ListenPrim' w)@ instance (if possible).
-- 'threadListenPrim' and 'threadListenPrimViaClass' can help you with that.
--
-- The following threading constraints accept 'ListenPrim':
--
-- * 'Control.Effect.ReaderThreads'
-- * 'Control.Effect.State.StateThreads'
-- * 'Control.Effect.State.StateLazyThreads'
-- * 'Control.Effect.Error.ErrorThreads'
-- * 'Control.Effect.Writer.WriterThreads'
-- * 'Control.Effect.Writer.WriterLazyThreads'
-- * 'Control.Effect.NonDet.NonDetThreads'
-- * 'Control.Effect.Stepped.SteppedThreads'
-- * 'Control.Effect.Cont.ContThreads'
-- * 'Control.Effect.Select.SelectThreads'
data ListenPrim w m a where
  ListenPrimTell   :: w -> ListenPrim w m ()
  ListenPrimListen :: m a -> ListenPrim w m (w, a)

-- | Construct a valid definition of 'threadEff' for a
-- @'ThreadsEff' t ('ListenPrim' w)@ instance
-- only be specifying how 'ListenPrimListen' should be lifted.
--
-- This uses 'lift' to lift 'ListenPrimTell'.
threadListenPrim :: forall w t m a
                  . (MonadTrans t, Monad m)
                 => ( forall x
                     . (forall y. ListenPrim w m y -> m y)
                    -> t m x -> t m (w, x)
                    )
                 -> (forall x. ListenPrim w m x -> m x)
                 -> ListenPrim w (t m) a -> t m a
threadListenPrim h alg = \case
  ListenPrimTell w   -> lift (alg (ListenPrimTell w))
  ListenPrimListen m -> h alg m
{-# INLINE threadListenPrim #-}

instance ( Reifies s (ReifiedEffAlgebra (ListenPrim w) m)
         , Monoid w
         , Monad m
         )
      => MonadWriter w (ViaAlg s (ListenPrim w) m) where
  tell w = case reflect @s of
    ReifiedEffAlgebra alg -> coerceAlg alg (ListenPrimTell w)

  pass = error "threadListenViaClass: Transformers threading ListenPrim \
                 \are not allowed to use pass."

  listen m = case reflect @s of
    ReifiedEffAlgebra alg ->
      fmap (\(s, a) -> (a, s)) $ coerceAlg alg (ListenPrimListen m)
  {-# INLINE listen #-}

-- | A valid definition of 'threadEff' for a @'ThreadsEff' t ('ListenPrim' w)@ instance,
-- given that @t@ lifts @'MonadWriter' w@.
--
-- __BEWARE__: 'threadListenViaClass' is only safe if the implementation of
-- 'listen' for @t m@ only makes use of 'listen' and 'tell' for @m@, and not
-- 'pass'.
threadListenPrimViaClass :: forall w t m a
                          . (Monoid w, Monad m)
                         => ( RepresentationalT t
                            , MonadTrans t
                            , forall b. MonadWriter w b => MonadWriter w (t b)
                            )
                         => (forall x. ListenPrim w m x -> m x)
                         -> ListenPrim w (t m) a -> t m a
threadListenPrimViaClass alg = \case
  ListenPrimTell w -> lift $ alg (ListenPrimTell w)
  ListenPrimListen m ->
    reify (ReifiedEffAlgebra alg) $ \(_ :: pr s) ->
        unViaAlgT
      $ fmap (\(a, s) -> (s, a))
      $ listen
      $ viaAlgT @s @(ListenPrim w) m
{-# INLINE threadListenPrimViaClass #-}

#define THREAD_LISTENPRIM(monadT)                              \
instance Monoid threadedMonoid                                 \
      => ThreadsEff (monadT) (ListenPrim threadedMonoid) where \
  threadEff = threadListenPrimViaClass;                        \
  {-# INLINE threadEff #-}

THREAD_LISTENPRIM(ReaderT i)
THREAD_LISTENPRIM(ExceptT e)
THREAD_LISTENPRIM(LSt.StateT s)
THREAD_LISTENPRIM(SSt.StateT s)

instance Monoid s => ThreadsEff (LWr.WriterT s) (ListenPrim w) where
  threadEff = threadListenPrim $ \alg m ->
      LWr.WriterT
    $ fmap (\(s, (a, w)) -> ((s, a), w))
    $ alg
    $ ListenPrimListen
    $ LWr.runWriterT m
  {-# INLINE threadEff #-}

instance Monoid s => ThreadsEff (SWr.WriterT s) (ListenPrim w) where
  threadEff = threadListenPrim $ \alg m ->
      SWr.WriterT
    $ fmap (\(s, (a, w)) -> ((s, a), w))
    $ alg
    $ ListenPrimListen
    $ SWr.runWriterT m
  {-# INLINE threadEff #-}

instance Monoid s => ThreadsEff (CPSWr.WriterT s) (ListenPrim w) where
  threadEff = threadListenPrim $ \alg m ->
      CPSWr.writerT
    $ fmap (\(s, (a, w)) -> ((s, a), w))
    $ alg
    $ ListenPrimListen
    $ CPSWr.runWriterT m
  {-# INLINE threadEff #-}
