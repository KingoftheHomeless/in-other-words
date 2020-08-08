{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Pass where

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

-- | An effect for altering what a computation
-- 'Control.Effect.Writer.tell's.
--
-- __'Pass' is typically used as a primitive effect.__
-- If you define a 'Control.Effect.Carrier' that relies on a novel
-- non-trivial monad transformer, then you need to make a
-- a @'Monoid' s => 'ThreadsEff' ('Pass' s)@ instance for that monad transformer
-- (if possible). 'threadPassViaClass' can help you with that.
newtype Pass s m a where
  Pass :: m (s -> s, a) -> Pass s m a

instance ( Reifies s (ReifiedEffAlgebra (Pass w) m)
         , Monoid w
         , Monad m
         )
      => MonadWriter w (ViaAlg s (Pass w) m) where
  tell = error "threadPassViaClass: Transformers threading Pass \
                \are not allowed to use tell."

  listen = error "threadPassViaClass: Transformers threading Pass \
                 \are not allowed to use listen."

  pass m = case reflect @s of
    ReifiedEffAlgebra alg ->
      coerceAlg alg (Pass (fmap (\(a,f) -> (f, a)) m))
  {-# INLINE pass #-}

-- | A valid definition of 'threadEff' for a @'ThreadsEff' ('Pass' w) t@ instance,
-- given that @t@ lifts @'MonadWriter' w@.
--
-- __BEWARE__: 'threadPassViaClass' is only safe if the implementation of
-- 'pass' for @t m@ only makes use of 'pass' for @m@, and no other methods
-- of 'MonadWriter'.
threadPassViaClass :: forall w t m a
                    . (Monoid w, Monad m)
                   => ( RepresentationalT t
                      , forall b. MonadWriter w b => MonadWriter w (t b)
                      )
                   => (forall x. Pass w m x -> m x)
                   -> Pass w (t m) a -> t m a
threadPassViaClass alg (Pass m) =
  reify (ReifiedEffAlgebra alg) $ \(_ :: pr s) ->
      unViaAlgT
    $ pass
    $ fmap (\(f, a) -> (a, f))
    $ viaAlgT @s @(Pass w) m
{-# INLINE threadPassViaClass #-}

#define THREAD_PASS(monadT)                              \
instance Monoid threadedMonoid                           \
      => ThreadsEff (Pass threadedMonoid) (monadT) where \
  threadEff = threadPassViaClass;                        \
  {-# INLINE threadEff #-}

THREAD_PASS(ReaderT i)
THREAD_PASS(ExceptT e)
THREAD_PASS(LSt.StateT s)
THREAD_PASS(SSt.StateT s)

instance ThreadsEff (Pass w) (LWr.WriterT s) where
  threadEff alg (Pass m) =
      LWr.WriterT
    $ alg
    $ Pass
    $ fmap (\((f,a), s) -> (f, (a, s)))
    $ LWr.runWriterT m
  {-# INLINE threadEff #-}

instance ThreadsEff (Pass w) (SWr.WriterT s) where
  threadEff alg (Pass m) =
      SWr.WriterT
    $ alg
    $ Pass
    $ fmap (\((f,a), s) -> (f, (a, s)))
    $ SWr.runWriterT m
  {-# INLINE threadEff #-}

instance Monoid s => ThreadsEff (Pass w) (CPSWr.WriterT s) where
  threadEff alg (Pass m) =
      CPSWr.writerT
    $ alg
    $ Pass
    $ fmap (\((f,a), s) -> (f, (a, s)))
    $ CPSWr.runWriterT m
  {-# INLINE threadEff #-}
