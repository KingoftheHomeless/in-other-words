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
-- non-trivial monad transformer @t@, then you need to make
-- a @'Monoid' o => 'ThreadsEff' t ('ListenPrim' o)@ instance (if possible).
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
data ListenPrim o :: Effect where
  ListenPrimTell   :: o -> ListenPrim o m ()
  ListenPrimListen :: m a -> ListenPrim o m (o, a)

-- | Construct a valid definition of 'threadEff' for a
-- @'ThreadsEff' t ('ListenPrim' o)@ instance
-- only be specifying how 'ListenPrimListen' should be lifted.
--
-- This uses 'lift' to lift 'ListenPrimTell'.
threadListenPrim :: forall o t m a
                  . (MonadTrans t, Monad m)
                 => ( forall x
                     . (forall y. ListenPrim o m y -> m y)
                    -> t m x -> t m (o, x)
                    )
                 -> (forall x. ListenPrim o m x -> m x)
                 -> ListenPrim o (t m) a -> t m a
threadListenPrim h alg = \case
  ListenPrimTell o   -> lift (alg (ListenPrimTell o))
  ListenPrimListen m -> h alg m
{-# INLINE threadListenPrim #-}

instance ( Reifies s (ReifiedEffAlgebra (ListenPrim o) m)
         , Monoid o
         , Monad m
         )
      => MonadWriter o (ViaAlg s (ListenPrim o) m) where
  tell o = case reflect @s of
    ReifiedEffAlgebra alg -> coerceAlg alg (ListenPrimTell o)

  pass = error "threadListenPrimViaClass: Transformers threading ListenPrim \
                 \are not allowed to use pass."

  listen m = case reflect @s of
    ReifiedEffAlgebra alg ->
      fmap (\(s, a) -> (a, s)) $ coerceAlg alg (ListenPrimListen m)
  {-# INLINE listen #-}

-- | A valid definition of 'threadEff' for a @'ThreadsEff' t ('ListenPrim' o)@
-- instance, given that @t@ lifts @'MonadWriter' w@.
--
-- __BEWARE__: 'threadListenPrimViaClass' is only safe if the implementation of
-- 'listen' for @t m@ only makes use of 'listen' and 'tell' for @m@, and not
-- 'pass'.
threadListenPrimViaClass :: forall o t m a
                          . (Monoid o, Monad m)
                         => ( RepresentationalT t
                            , MonadTrans t
                            , forall b. MonadWriter o b => MonadWriter o (t b)
                            )
                         => (forall x. ListenPrim o m x -> m x)
                         -> ListenPrim o (t m) a -> t m a
threadListenPrimViaClass alg = \case
  ListenPrimTell o -> lift $ alg (ListenPrimTell o)
  ListenPrimListen m ->
    reify (ReifiedEffAlgebra alg) $ \(_ :: pr s) ->
        unViaAlgT
      $ fmap (\(a, o) -> (o, a))
      $ listen
      $ viaAlgT @s @(ListenPrim o) m
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

instance Monoid s => ThreadsEff (LWr.WriterT s) (ListenPrim o) where
  threadEff = threadListenPrim $ \alg m ->
      LWr.WriterT
    $ fmap (\(s, (a, o)) -> ((s, a), o))
    $ alg
    $ ListenPrimListen
    $ LWr.runWriterT m
  {-# INLINE threadEff #-}

instance Monoid s => ThreadsEff (SWr.WriterT s) (ListenPrim o) where
  threadEff = threadListenPrim $ \alg m ->
      SWr.WriterT
    $ fmap (\(s, (a, o)) -> ((s, a), o))
    $ alg
    $ ListenPrimListen
    $ SWr.runWriterT m
  {-# INLINE threadEff #-}

instance Monoid s => ThreadsEff (CPSWr.WriterT s) (ListenPrim o) where
  threadEff = threadListenPrim $ \alg m ->
      CPSWr.writerT
    $ fmap (\(s, (a, o)) -> ((s, a), o))
    $ alg
    $ ListenPrimListen
    $ CPSWr.runWriterT m
  {-# INLINE threadEff #-}
