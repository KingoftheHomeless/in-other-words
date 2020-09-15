module Control.Effect.Reader
  ( -- * Effects
    Ask(..)
  , Local(..)
  , Reader

    -- * Actions
  , ask
  , local

    -- * Interpretations
  , runAskConst

  , runAskAction

  , askToAsk

  , runReader

    -- * Simple variants
  , runAskConstSimple
  , runAskActionSimple
  , askToAskSimple

    -- * Threading constraints
  , ReaderThreads

    -- * Carriers
  , ReaderC
  ) where

import Control.Effect

import Control.Effect.Internal.Reader

import Control.Monad.Trans.Reader (ReaderT(..))


ask :: Eff (Ask i) m => m i
ask = send Ask
{-# INLINE ask #-}

asks :: Eff (Ask i) m => (i -> a) -> m a
asks = (<$> ask)
{-# INLINE asks #-}

local :: Eff (Local i) m => (i -> i) -> m a -> m a
local f m = send (Local f m)
{-# INLINE local #-}


-- | Run connected @'Ask' i@ and @'Local' i@ effects -- i.e. @'Reader' i@.
--
-- @'Derivs' ('ReaderC' i m) = 'Local' i ': 'Ask' i ': 'Derivs' m@
--
-- @'Control.Effect.Carrier.Prims'  ('ReaderC' i m) = 'Control.Effect.Type.ReaderPrim.ReaderPrim' i ': 'Control.Effect.Carrier.Prims' m@
runReader :: forall i m a p
           . ( Carrier m
             , Threaders '[ReaderThreads] m p
             )
          => i
          -> ReaderC i m a
          -> m a
runReader i m = runReaderT (unReaderC m) i
{-# INLINE runReader #-}

-- | Run an 'Ask' effect by providing a constant to be given
-- at each use of 'ask'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'runAskConst' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower 'runAskConstSimple',
-- which doesn't have a higher-rank type.
runAskConst :: forall i m a
             . Carrier m
            => i
            -> InterpretReifiedC (Ask i) m a
            -> m a
runAskConst i = interpret $ \case
  Ask -> return i
{-# INLINE runAskConst #-}

-- | Run an 'Ask' effect by providing an action to be executed
-- at each use of 'ask'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'runAskAction' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower 'runAskActionSimple',
-- which doesn't have a higher-rank type.
runAskAction :: forall i m a
              . Carrier m
             => m i
             -> InterpretReifiedC (Ask i) m a
             -> m a
runAskAction m = interpret $ \case
  Ask -> liftBase m
{-# INLINE runAskAction #-}

-- | Transform an @'Ask' i@ effect into an @'Ask' j@ effect by
-- providing a function to convert @j@ to @i@.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'askToAsk' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower 'askToAskSimple',
-- which doesn't have a higher-rank type.
askToAsk :: forall i j m a
          . Eff (Ask j) m
         => (j -> i)
         -> InterpretReifiedC (Ask i) m a
         -> m a
askToAsk f = interpret $ \case
  Ask -> asks f
{-# INLINE askToAsk #-}

-- | Run an 'Ask' effect by providing a constant to be given
-- at each use of 'ask'
--
-- This is a less performant version of 'runAskConst' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
runAskConstSimple :: forall i m a p
                   . ( Carrier m
                     , Threaders '[ReaderThreads] m p
                     )
                  => i
                  -> InterpretSimpleC (Ask i) m a
                  -> m a
runAskConstSimple i = interpretSimple $ \case
  Ask -> return i
{-# INLINE runAskConstSimple #-}

-- | Run an 'Ask' effect by providing an action to be executed
-- at each use of 'ask'.
--
-- This is a less performant version of 'runAskAction' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
runAskActionSimple :: forall i m a p
                    . ( Carrier m
                      , Threaders '[ReaderThreads] m p
                      )
                   => m i
                   -> InterpretSimpleC (Ask i) m a
                   -> m a
runAskActionSimple mi = interpretSimple $ \case
  Ask -> liftBase mi
{-# INLINE runAskActionSimple #-}

-- | Transform an @'Ask' i@ effect into an @'Ask' j@ effect by
-- providing a function to convert @j@ to @i@.
--
-- This is a less performant version of 'askToAsk' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
askToAskSimple :: forall i j m a p
                . ( Eff (Ask j) m
                  , Threaders '[ReaderThreads] m p
                  )
               => (j -> i)
               -> InterpretSimpleC (Ask i) m a
               -> m a
askToAskSimple f = interpretSimple $ \case
  Ask -> asks f
{-# INLINE askToAskSimple #-}
