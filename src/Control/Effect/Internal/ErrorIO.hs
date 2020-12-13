{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.ErrorIO where

import Control.Monad

import Control.Effect
import Control.Effect.Optional
import Control.Effect.Type.ErrorIO

import Control.Exception (SomeException)
import qualified Control.Exception as X
import qualified Control.Monad.Catch as C

-- For coercion purposes
import Control.Effect.Internal.Derive
import Control.Effect.Carrier.Internal.Intro
import Control.Effect.Carrier.Internal.Compose
import Control.Effect.Carrier.Internal.Interpret
import Control.Effect.Internal.Utils

newtype ErrorIOToIOC m a = ErrorIOToIOC {
    unErrorIOToIOC ::
        ReinterpretC ErrorIOFinalH ErrorIO
         '[Optional ((->) SomeException)]
      ( InterpretPrimC ErrorIOFinalH (Optional ((->) SomeException))
      ( m
      )) a
  } deriving ( Functor, Applicative, Monad
             , Alternative, MonadPlus
             , MonadFix, MonadFail, MonadIO
             , MonadThrow, MonadCatch, MonadMask
             , MonadBase b, MonadBaseControl b
             )
    deriving (MonadTrans, MonadTransControl)
    via CompositionBaseT
     '[ ReinterpretC ErrorIOFinalH ErrorIO
         '[Optional ((->) SomeException)]
      , InterpretPrimC ErrorIOFinalH (Optional ((->) SomeException))
      ]

deriving instance (Carrier m, C.MonadCatch m)
               => Carrier (ErrorIOToIOC m)

-- | Run an @'ErrorIO'@ effect by making use of 'IO' exceptions.
--
-- @'Derivs' (ErrorIOToIOC e m) = 'ErrorIO' ': 'Derivs' m@
--
-- @'Control.Effect.Carrier.Prims' (ErrorIOToIOC e m) = 'Control.Effect.Optional.Optional' ((->) 'SomeException') ': 'Control.Effect.Carrier.Prims' m@
errorIOToIO :: (Carrier m, C.MonadCatch m)
            => ErrorIOToIOC m a
            -> m a
errorIOToIO =
     interpretPrimViaHandler
  .# reinterpretViaHandler
  .# unErrorIOToIOC
{-# INLINE errorIOToIO #-}

data ErrorIOFinalH

instance ( C.MonadThrow m
         , Eff (Optional ((->) SomeException)) m
         )
      => Handler ErrorIOFinalH ErrorIO m where
  effHandler = \case
    ThrowIO x   -> liftBase $ C.throwM x
    CatchIO m h -> join $
      optionally
        (\x -> case X.fromException x of
            Just e -> h e
            Nothing -> liftBase $ C.throwM x
        )
        (fmap pure m)
  {-# INLINEABLE effHandler #-}

instance ( C.MonadCatch m
         , Carrier m
         )
      => PrimHandler ErrorIOFinalH (Optional ((->) SomeException)) m where
  effPrimHandler = \case
    Optionally h m -> m `C.catch` (return . h)
  {-# INLINEABLE effPrimHandler #-}
