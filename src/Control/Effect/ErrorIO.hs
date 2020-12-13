{-# LANGUAGE DerivingVia #-}
module Control.Effect.ErrorIO
  ( -- * Effects
    ErrorIO(..)
  , X.Exception(..)
  , SomeException

    -- * Actions
  , throwIO
  , catchIO

    -- * Interpretations
  , errorIOToIO

  , errorIOToError

    -- * MonadCatch
  , C.MonadCatch

    -- * Carriers
  , ErrorIOToIOC
  , ErrorIOToErrorC
  ) where

import Control.Effect
import Control.Effect.Type.ErrorIO
import Control.Effect.Type.Throw
import Control.Effect.Type.Catch

import Control.Effect.Internal.ErrorIO

import Control.Exception (SomeException)
import qualified Control.Exception as X
import qualified Control.Monad.Catch as C

throwIO :: (X.Exception e, Eff ErrorIO m) => e -> m a
throwIO = send . ThrowIO

catchIO :: (X.Exception e, Eff ErrorIO m) => m a -> (e -> m a) -> m a
catchIO m h = send (CatchIO m h)

data ErrorIOToErrorH

instance ( Eff (Error SomeException) m
         , Carrier m
         )
      => Handler ErrorIOToErrorH ErrorIO m where
  effHandler = \case
    ThrowIO e -> send $ Throw (X.toException e)
    CatchIO m h -> send $ Catch m $ \e -> case X.fromException e of
      Just e' -> h e'
      _       -> send $ Throw e
  {-# INLINEABLE effHandler #-}


type ErrorIOToErrorC = InterpretC ErrorIOToErrorH ErrorIO

-- | Transform an @'ErrorIO'@ effect into an @'Error' 'SomeException'@
-- effect.
errorIOToError :: Eff (Error SomeException) m
               => ErrorIOToErrorC m a
               -> m a
errorIOToError = interpretViaHandler
{-# INLINE errorIOToError #-}
