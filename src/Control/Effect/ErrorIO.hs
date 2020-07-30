module Control.Effect.ErrorIO
  ( -- * Effects
    ErrorIO(..)
  , X.Exception(..)
  , SomeException

    -- * Actions
  , throwIO
  , catchIO

    -- * Interpretations
  , ErrorIOToIOC
  , errorIOToIO

  , ErrorIOToErrorC
  , errorIOToError
  ) where

import Control.Monad

import Control.Effect
import Control.Effect.Optional
import Control.Effect.Type.ErrorIO
import Control.Effect.Type.Throw
import Control.Effect.Type.Catch

import Control.Exception (SomeException)
import qualified Control.Exception as X
import qualified Control.Monad.Catch as C

-- For coercion purposes
import Control.Monad.Trans.Identity
import Control.Effect.Carrier.Internal.Intro
import Control.Effect.Carrier.Internal.Compose
import Control.Effect.Carrier.Internal.Interpret
import Control.Effect.Internal.Utils

throwIO :: (X.Exception e, Eff ErrorIO m) => e -> m a
throwIO = send . ThrowIO

catchIO :: (X.Exception e, Eff ErrorIO m) => m a -> (e -> m a) -> m a
catchIO m h = send (CatchIO m h)

data ErrorIOFinalH

data ErrorIOToErrorH

instance ( C.MonadThrow m
         , Eff (Optional ((->) SomeException)) m
         )
      => Handler ErrorIOFinalH ErrorIO m where
  effHandler = \case
    ThrowIO x   -> liftBase $ C.throwM x
    CatchIO m h -> join $
      optional
        (\x -> case X.fromException x of
            Just e -> h e
            Nothing -> liftBase $ C.throwM x
        )
        (fmap pure m)
  {-# INLINE effHandler #-}

instance ( C.MonadCatch m
         , Carrier m
         )
      => PrimHandler ErrorIOFinalH (Optional ((->) SomeException)) m where
  effPrimHandler = \case
    Optional h m -> m `C.catch` (return . h)
  {-# INLINE effPrimHandler #-}


instance ( Eff (Error SomeException) m
         , Carrier m
         )
      => Handler ErrorIOToErrorH ErrorIO m where
  effHandler = \case
    ThrowIO e -> send $ Throw (X.toException e)
    CatchIO m h -> send $ Catch m $ \e -> case X.fromException e of
      Just e' -> h e'
      _       -> send $ Throw e
  {-# INLINE effHandler #-}


type ErrorIOToIOC = CompositionC
 '[ ReinterpretC ErrorIOFinalH ErrorIO
     '[Optional ((->) SomeException)]
  , InterpretPrimC ErrorIOFinalH (Optional ((->) SomeException))
  ]

type ErrorIOToErrorC = InterpretC ErrorIOToErrorH ErrorIO

-- | Transform an @'ErrorIO'@ effect into an @'Error' 'SomeException'@
-- effect.
errorIOToError :: Eff (Error SomeException) m
               => ErrorIOToErrorC m a
               -> m a
errorIOToError = interpretViaHandler
{-# INLINE errorIOToError #-}

-- | Run an @'ErrorIO'@ effect by making use of 'IO' exceptions.
--
-- @'Derivs' (ErrorIOToIOC e m) = 'ErrorIO' ': 'Derivs' m@
-- @'Prims' (ErrorIOToIOC e m) = 'Optional' ((->) SomeException) ': 'Prims' m@
errorIOToIO :: (Carrier m, C.MonadCatch m)
            => ErrorIOToIOC m a
            -> m a
errorIOToIO =
     interpretPrimViaHandler
  .# reinterpretViaHandler
  .# runComposition
{-# INLINE errorIOToIO #-}
