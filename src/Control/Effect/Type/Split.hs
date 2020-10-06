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
-- a @'ThreadsEff'@ instance for that monad transformer
-- to lift 'Split' (if possible).
--
-- The following threading constraints accept 'Split':
--
-- * 'Control.Effect.ReaderThreads'
-- * 'Control.Effect.State.StateThreads'
-- * 'Control.Effect.State.StateLazyThreads'
-- * 'Control.Effect.Writer.WriterThreads'
-- * 'Control.Effect.Writer.WriterLazyThreads'
data Split m a where
  Split :: (Maybe (a, m a) -> b) -> m a -> Split m b

instance ThreadsEff (ReaderT s) Split where
  threadEff alg (Split c m) = ReaderT $ \s ->
    alg $ Split (c . (fmap . fmap) lift) (runReaderT m s)
  {-# INLINE threadEff #-}

instance ThreadsEff (LSt.StateT s) Split where
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

instance ThreadsEff (SSt.StateT s) Split where
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

instance Monoid s => ThreadsEff (LWr.WriterT s) Split where
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

instance Monoid s => ThreadsEff (SWr.WriterT s) Split where
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

instance Monoid s => ThreadsEff (CPSWr.WriterT s) Split where
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
