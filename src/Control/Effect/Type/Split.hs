{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Split where

import Control.Effect.Internal.Union
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Lazy    as LSt
import qualified Control.Monad.Trans.State.Strict  as SSt
import qualified Control.Monad.Trans.Writer.Lazy   as LWr
import qualified Control.Monad.Trans.Writer.Strict as SWr
import qualified Control.Monad.Trans.Writer.CPS    as CPSWr

-- | An effect for spliting a nondeterministic computation
-- into its head and tail.
--
-- __'Split' is typically used as a primitive effect.__
-- If you define a 'Control.Effect.Carrier' that relies on a novel
-- non-trivial monad transformer, then you need to make a
-- a @'ThreadsEff' 'Split'@ instance for that monad transformer
-- (if possible).
data Split m a where
  Split :: (Maybe (a, m a) -> b) -> m a -> Split m b

instance ThreadsEff Split (ReaderT s) where
  threadEff alg (Split c m) = ReaderT $ \s ->
    alg $ Split (c . (fmap . fmap) lift) (runReaderT m s)
  {-# INLINE threadEff #-}

instance ThreadsEff Split (LSt.StateT s) where
  threadEff alg (Split c m) = LSt.StateT $ \s ->
    alg $
      Split
        (maybe
          (c Nothing, s)
          (\ ~( ~(a, s'), m') ->
             (c $ Just (a, LSt.StateT $ \_ -> m'), s')
          )
        )
        (LSt.runStateT m s)
  {-# INLINE threadEff #-}

instance ThreadsEff Split (SSt.StateT s) where
  threadEff alg (Split c m) = SSt.StateT $ \s ->
    alg $
      Split
        (maybe
          (c Nothing, s)
          (\((a, s'), m') ->
             (c $ Just (a, SSt.StateT $ \_ -> m'), s')
          )
        )
        (SSt.runStateT m s)
  {-# INLINE threadEff #-}

instance Monoid s => ThreadsEff Split (LWr.WriterT s) where
  threadEff alg (Split c m) = LWr.WriterT $
    alg $
      Split
        (maybe
          (c Nothing, mempty)
          (\ ~( ~(a, s'), m') ->
             (c $ Just (a, LWr.WriterT m'), s')
          )
        )
        (LWr.runWriterT m)
  {-# INLINE threadEff #-}

instance Monoid s => ThreadsEff Split (SWr.WriterT s) where
  threadEff alg (Split c m) = SWr.WriterT $
    alg $
      Split
        (maybe
          (c Nothing, mempty)
          (\((a, s'), m') ->
             (c $ Just (a, SWr.WriterT m'), s')
          )
        )
        (SWr.runWriterT m)
  {-# INLINE threadEff #-}

instance Monoid s => ThreadsEff Split (CPSWr.WriterT s) where
  threadEff alg (Split c m) = CPSWr.writerT $
    alg $
      Split
        (maybe
          (c Nothing, mempty)
          (\((a, s'), m') ->
             (c $ Just (a, CPSWr.writerT m'), s')
          )
        )
        (CPSWr.runWriterT m)
  {-# INLINE threadEff #-}
