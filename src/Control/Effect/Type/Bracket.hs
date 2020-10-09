{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Bracket
 ( -- * Effects
   Bracket(..)
 , ExitCase(..)

   -- * Threading utilities
 , threadBracketViaClass
 ) where

import Control.Effect.Internal.Union
import Control.Effect.Internal.Utils
import Control.Effect.Internal.Reflection
import Control.Effect.Internal.ViaAlg
-- import qualified Control.Exception as X
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask, ExitCase(..))
import qualified Control.Monad.Catch as C
-- import Control.Applicative
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.State.Strict as SSt
import qualified Control.Monad.Trans.State.Lazy as LSt
import qualified Control.Monad.Trans.Writer.Lazy as LWr
import qualified Control.Monad.Trans.Writer.Strict as SWr
import qualified Control.Monad.Trans.Writer.CPS as CPSWr


-- | An effect for exception-safe acquisition and release of resources.
--
-- __'Bracket' is typically used as a primitive effect__.
-- If you define a 'Control.Effect.Carrier' that relies on a novel
-- non-trivial monad transformer @t@, then you need to make
-- a @'ThreadsEff' t 'Bracket'@ instance (if possible).
-- 'threadBracketViaClass' can help you with that.
--
-- The following threading constraints accept 'Bracket':
--
-- * 'Control.Effect.ReaderThreads'
-- * 'Control.Effect.State.StateThreads'
-- * 'Control.Effect.State.StateLazyThreads'
-- * 'Control.Effect.Error.ErrorThreads'
-- * 'Control.Effect.Writer.WriterThreads'
-- * 'Control.Effect.Writer.WriterLazyThreads'
data Bracket m a where
  GeneralBracket :: m a
                 -> (a -> ExitCase b -> m c)
                 -> (a -> m b)
                 -> Bracket m (b, c)

instance Monad m => MonadThrow (ViaAlg s Bracket m) where
  throwM = error "threadBracketViaClass: Transformers threading Bracket \
                 \are not allowed to use throwM."
instance Monad m => MonadCatch (ViaAlg s Bracket m) where
  catch = error "threadBracketViaClass: Transformers threading Bracket \
                 \are not allowed to use catch."

instance ( Reifies s (ReifiedEffAlgebra Bracket m)
         , Monad m
         )
       => MonadMask (ViaAlg s Bracket m) where
  mask m = m id
  uninterruptibleMask m = m id

  generalBracket acquire release use = case reflect @s of
    ReifiedEffAlgebra alg -> coerceAlg alg (GeneralBracket acquire release use)
  {-# INLINE generalBracket #-}

-- | A valid definition of 'threadEff' for a @'ThreadsEff' t 'Bracket'@ instance,
-- given that @t@ lifts @'MonadMask'@.
--
-- __BEWARE__: 'threadBracketViaClass' is only safe if the implementation of
-- 'C.generalBracket' for @t m@ only makes use of 'C.generalBracket' for @m@,
-- and no other methods of 'MonadThrow', 'MonadCatch', or 'MonadMask'.
threadBracketViaClass :: forall t m a
                       . Monad m
                      => ( RepresentationalT t
                         , forall b. MonadMask b => MonadMask (t b)
                         )
                      => (forall x. Bracket m x -> m x)
                      -> Bracket (t m) a -> t m a
threadBracketViaClass alg (GeneralBracket acquire release use) =
  reify (ReifiedEffAlgebra alg) $ \(_ :: pr s) ->
    unViaAlgT @s @Bracket $
      C.generalBracket
        (viaAlgT acquire)
        ((viaAlgT .) #. release)
        (viaAlgT #. use)
{-# INLINE threadBracketViaClass #-}


#define THREAD_BRACKET(monadT)             \
instance ThreadsEff (monadT) Bracket where \
  threadEff = threadBracketViaClass;       \
  {-# INLINE threadEff #-}

#define THREAD_BRACKET_CTX(ctx, monadT)             \
instance (ctx) => ThreadsEff (monadT) Bracket where \
  threadEff = threadBracketViaClass;                \
  {-# INLINE threadEff #-}


THREAD_BRACKET(ReaderT i)
THREAD_BRACKET(ExceptT e)
THREAD_BRACKET(LSt.StateT s)
THREAD_BRACKET(SSt.StateT s)
THREAD_BRACKET_CTX(Monoid s, LWr.WriterT s)
THREAD_BRACKET_CTX(Monoid s, SWr.WriterT s)

instance Monoid s => ThreadsEff (CPSWr.WriterT s) Bracket where
  threadEff alg (GeneralBracket acq rel use) = CPSWr.writerT $
      fmap (\( (b,sUse), (c,sEnd) ) -> ((b, c), sUse <> sEnd))
    . alg $
      GeneralBracket
        (CPSWr.runWriterT acq)
        (\(a, _) ec -> CPSWr.runWriterT $ rel a $ case ec of
          ExitCaseSuccess (b, _) -> ExitCaseSuccess b
          ExitCaseException exc  -> ExitCaseException exc
          ExitCaseAbort          -> ExitCaseAbort
        )
        (\(a, s) -> CPSWr.runWriterT (CPSWr.tell s >> use a))
  {-# INLINE threadEff #-}
