module Control.Effect.Input
  ( -- * Effect
    Input(..)

    -- * Actions
  , input
  , inputs

    -- Interpretations
  , runInputConst

  , runInputAction

  , inputToInput

  , InputToReaderC
  , inputToReader

    -- Simple variants
  , runInputConstSimple
  , runInputActionSimple
  , inputToInputSimple
  ) where

import Control.Effect
import Control.Effect.Reader
import Control.Effect.Type.Input

import Control.Effect.Carrier

import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as R

input :: Eff (Input i) m => m i
input = send Input
{-# INLINE input #-}

inputs :: Eff (Input i) m => (i -> a) -> m a
inputs = (<$> input)
{-# INLINE inputs #-}

-- | Run an 'Input' effect by providing a constant to be given
-- at each use of 'input'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- **This makes 'runInputConst' very difficult to use partially applied.**
-- **In particular, it can't be composed using @'.'@.**
--
-- If performance is secondary, consider using the slower 'runInputConstSimple',
-- which doesn't have a higher-rank type.
runInputConst :: Carrier m
              => i
              -> InterpretReifiedC (Input i) m a
              -> m a
runInputConst i = interpret $ \case
  Input -> return i
{-# INLINE runInputConst #-}

-- | Run an 'Input' effect by providing an action to be executed
-- at each use of 'input'.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- **This makes 'runInputAction' very difficult to use partially applied.**
-- **In particular, it can't be composed using @'.'@.**
--
-- If performance is secondary, consider using the slower 'runInputActionSimple',
-- which doesn't have a higher-rank type.
runInputAction :: forall i m a
                . Carrier m
               => m i
               -> InterpretReifiedC (Input i) m a
               -> m a
runInputAction m = interpret $ \case
  Input -> liftBase m
{-# INLINE runInputAction #-}

-- | Transform an @'Input' i@ effect into an @'Input' j@ effect by
-- providing a function to convert @j@ to @i@.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- **This makes 'inputToInput' very difficult to use partially applied.**
-- **In particular, it can't be composed using @'.'@.**
--
-- If performance is secondary, consider using the slower 'inputToInputSimple',
-- which doesn't have a higher-rank type.
inputToInput :: forall i j m a
              . Eff (Input j) m
             => (j -> i)
             -> InterpretReifiedC (Input i) m a
             -> m a
inputToInput f = interpret $ \case
  Input -> inputs f
{-# INLINE inputToInput #-}

data InputToReaderH

instance Eff (Reader i) m
      => Handler InputToReaderH (Input i) m where
  effHandler Input = ask
  {-# INLINE effHandler #-}

type InputToReaderC i = InterpretC InputToReaderH (Input i)

-- | Transform an @'Input' i@ effect into a @'Reader' i@ effect.
inputToReader :: Eff (Reader i) m
              => InputToReaderC i m a
              -> m a
inputToReader = interpretViaHandler
{-# INLINE inputToReader #-}


newtype InputConstSimpleC i m a = InputConstSimpleC {
    unInputConstSimpleC :: ReaderT i m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl)

instance ( Threads (ReaderT i) (Prims m)
         , Carrier m
         )
      => Carrier (InputConstSimpleC i m) where
  type Derivs (InputConstSimpleC i m) = Input i ': Derivs m
  type Prims  (InputConstSimpleC i m) = Prims m

  algPrims = coerceAlg (thread @(ReaderT i) (algPrims @m))
  {-# INLINE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    Input -> n (InputConstSimpleC R.ask)
  {-# INLINE reformulate #-}

-- | Run an 'Input' effect by providing a constant to be given
-- at each use of 'input'
--
-- This is a less performant version of 'runInputConst' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
runInputConstSimple :: forall i m a p
                     . ( Carrier m
                       , Threaders '[ReaderThreads] m p
                       )
                    => i
                    -> InputConstSimpleC i m a
                    -> m a
runInputConstSimple i m = runReaderT (unInputConstSimpleC m) i
{-# INLINE runInputConstSimple #-}

-- | Run an 'Input' effect by providing an action to be executed
-- at each use of 'input'.
--
-- This is a less performant version of 'runInputAction' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
runInputActionSimple :: forall i m a p
                      . ( Carrier m
                        , Threaders '[ReaderThreads] m p
                        )
                     => m i
                     -> InterpretSimpleC (Input i) m a
                     -> m a
runInputActionSimple mi = interpretSimple $ \case
  Input -> liftBase mi
{-# INLINE runInputActionSimple #-}

-- | Transform an @'Input' i@ effect into an @'Input' j@ effect by
-- providing a function to convert @j@ to @i@.
--
-- This is a less performant version of 'inputToInput' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
inputToInputSimple :: forall i j m a p
                    . ( Eff (Input j) m
                      , Threaders '[ReaderThreads] m p
                      )
                   => (j -> i)
                   -> InterpretSimpleC (Input i) m a
                   -> m a
inputToInputSimple f = interpretSimple $ \case
  Input -> inputs f
{-# INLINE inputToInputSimple #-}
