{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Exceptional where

import Control.Effect
import Control.Effect.Error
import Control.Effect.ErrorIO
import Control.Effect.Union

import Control.Effect.Carrier

import Control.Effect.Internal.Utils

-- For coercion purposes
import Control.Monad.Trans.Except
import Control.Effect.Internal.Error
import Control.Effect.Carrier.Internal.Interpret
import Control.Effect.Carrier.Internal.Intro
import Control.Effect.Carrier.Internal.Compose


-- | An effect that allows for the safe use of an effect @eff@ that may
-- throw exceptions of the type @exc@ by forcing the user to eventually
-- catch those exceptions at some point of the program.
--
-- The main combinator of 'Exceptional' is 'Control.Effect.Exceptional.catching'.
--
-- __This could be unsafe in the presence of 'Control.Effect.Conc.Conc'__.
-- If you use 'Control.Effect.Exceptional.catching' on a computation that:
--
-- * Spawns an asynchronous computation
-- * Throws an exception inside the asynchronous computation from a use of @eff@
-- * Returns the 'Control.Effect.Conc.Async' of that asynchronous computation
--
-- Then 'Control.Effect.Conc.wait'ing on that 'Control.Effect.Conc.Async'
-- outside of the 'Control.Effect.Exceptional.catching' will throw that exception
-- without it being caught.
newtype Exceptional eff exc m a = Exceptional (Union '[eff, Catch exc] m a)

-- | A particularly useful specialization of 'Exceptional', for gaining
-- restricted access to an @'Error' exc@ effect.
-- Main combinators are 'Control.Effect.Exceptional.catchSafe' and
-- 'Control.Effect.Exceptional.trySafe'.
type SafeError exc = Exceptional (Throw exc) exc

{-
"ExceptionallyC" can easily be implemented using Handler:

data ExceptionallyH exc

instance ( Eff (Exceptional eff exc) m
         , RepresentationalEff eff
         )
      => Handler (ExceptionallH exc) eff m where where
  effHandler e = send $ Exceptionally $ inj e

type ExceptionallyC eff exc = InterpretC (ExceptionallH exc) eff

catching :: forall eff exc m a
          . ( Eff (Exceptional eff exc) m
            , RepresentationalEff eff
            )
         => ExceptionallyC exc eff m a
         -> (exc -> m a)
         -> m a
catching m h =
  send $ Exceptional @eff @exc $
    inj (Catch @exc (interpretViaHandler m) h)

We use a standalone carrier to hide the RepresentationalEff constraint,
which is just noise in this case.
-}

newtype ExceptionallyC (eff :: Effect) (exc :: *) m a = ExceptionallyC {
    unExceptionallyC :: m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance Eff (Exceptional eff exc) m
      => Carrier (ExceptionallyC eff exc m) where
  type Derivs (ExceptionallyC eff exc m) = Catch exc ': eff ': Derivs m
  type Prims  (ExceptionallyC eff exc m) = Prims m

  algPrims = coerce (algPrims @m)
  {-# INLINEABLE algPrims #-}

  reformulate n alg =
    powerAlg (
    powerAlg' (
      reformulate (n .# lift) alg
    ) $ \e ->
      reformulate (n .# lift) alg $ inj $
        Exceptional @eff @exc (Union Here e)
    ) $ \e ->
      reformulate (n .# lift) alg $ inj $
        Exceptional @eff @exc (Union (There Here) e)
  {-# INLINEABLE reformulate #-}

  algDerivs =
    powerAlg (
    powerAlg' (
      coerce (algDerivs @m)
    ) $ \e ->
      coerceAlg (algDerivs @m) $ inj $ Exceptional @eff @exc (Union Here e)
    ) $ \e ->
      coerceAlg (algDerivs @m) $ inj $ Exceptional @eff @exc (Union (There Here) e)
  {-# INLINEABLE algDerivs #-}

data ExceptionalH

instance ( Member eff (Derivs m)
         , Eff (Catch exc) m
         )
      => Handler ExceptionalH (Exceptional eff exc) m where
  -- Explicit pattern mathing and use of 'algDerivs' instead of using
  -- 'decomp' and 'send' so that we don't introduce the
  -- RepresentationalEff constraint.
  effHandler (Exceptional e) = case e of
    Union Here eff             -> algDerivs (Union membership eff)
    Union (There Here) eff     -> algDerivs (Union membership eff)
    Union (There (There pr)) _ -> absurdMember pr
  {-# INLINEABLE effHandler #-}

type ExceptionalC eff exc = InterpretC ExceptionalH (Exceptional eff exc)

type SafeErrorToErrorC exc = ExceptionalC (Throw exc) exc

newtype SafeErrorC exc m a = SafeErrorC {
    unSafeErrorC ::
        IntroUnderC (SafeError exc) '[Catch exc, Throw exc]
      ( SafeErrorToErrorC exc
      ( ErrorC exc
      ( m
      ))) a
  } deriving ( Functor, Applicative, Monad
             , Alternative, MonadPlus
             , MonadFix, MonadFail, MonadIO
             , MonadThrow, MonadCatch, MonadMask
             , MonadBase b, MonadBaseControl b
             )
    deriving (MonadTrans, MonadTransControl)
    via CompositionBaseT
     '[ IntroUnderC (SafeError exc) '[Catch exc, Throw exc]
      , SafeErrorToErrorC exc
      , ErrorC exc
      ]

deriving instance (Carrier m, Threads (ExceptT exc) (Prims m))
               => Carrier (SafeErrorC exc m)

newtype SafeErrorToIOC' s s' exc m a = SafeErrorToIOC' {
    unSafeErrorToIOC' ::
        IntroUnderC (SafeError exc) '[Catch exc, Throw exc]
      ( SafeErrorToErrorC exc
      ( ErrorToIOC' s s' exc
      ( m
      ))) a
  } deriving ( Functor, Applicative, Monad
             , Alternative, MonadPlus
             , MonadFix, MonadFail, MonadIO
             , MonadThrow, MonadCatch, MonadMask
             , MonadBase b, MonadBaseControl b
             )
    deriving (MonadTrans, MonadTransControl)
    via CompositionBaseT
     '[ IntroUnderC (SafeError exc) '[Catch exc, Throw exc]
      , SafeErrorToErrorC exc
      , ErrorToIOC' s s' exc
      ]

deriving instance ( Eff (Embed IO) m, MonadCatch m
                  , ReifiesErrorHandler s s' exc (ErrorIOToIOC m)
                  )
               => Carrier (SafeErrorToIOC' s s' exc m)

type SafeErrorToIOC e m a =
     forall s s'
   . ReifiesErrorHandler s s' e (ErrorIOToIOC m)
  => SafeErrorToIOC' s s' e m a

newtype SafeErrorToErrorIOC' s s' exc m a = SafeErrorToErrorIOC' {
    unSafeErrorToErrorIOC' ::
        IntroUnderC (SafeError exc) '[Catch exc, Throw exc]
      ( SafeErrorToErrorC exc
      ( InterpretErrorC' s s' exc
      ( m
      ))) a
  } deriving ( Functor, Applicative, Monad
             , Alternative, MonadPlus
             , MonadFix, MonadFail, MonadIO
             , MonadThrow, MonadCatch, MonadMask
             , MonadBase b, MonadBaseControl b
             )
    deriving (MonadTrans, MonadTransControl)
    via CompositionBaseT
     '[ IntroUnderC (SafeError exc) '[Catch exc, Throw exc]
      , SafeErrorToErrorC exc
      , InterpretErrorC' s s' exc
      ]

deriving instance (Carrier m, ReifiesErrorHandler s s' exc m)
               => Carrier (SafeErrorToErrorIOC' s s' exc m)

type SafeErrorToErrorIOC e m a =
     forall s s'
   . ReifiesErrorHandler s s' e m
  => SafeErrorToErrorIOC' s s' e m a

newtype SafeErrorToIOSimpleC exc m a = SafeErrorToIOSimpleC {
    unSafeErrorToIOSimpleC ::
        IntroUnderC (SafeError exc) '[Catch exc, Throw exc]
      ( SafeErrorToErrorC exc
      ( ErrorToIOSimpleC exc
      ( m
      ))) a
  } deriving ( Functor, Applicative, Monad
             , Alternative, MonadPlus
             , MonadFix, MonadFail, MonadIO
             , MonadThrow, MonadCatch, MonadMask
             , MonadBase b, MonadBaseControl b
             )
    deriving MonadTrans
    via CompositionBaseT
     '[ IntroUnderC (SafeError exc) '[Catch exc, Throw exc]
      , SafeErrorToErrorC exc
      , ErrorToIOSimpleC exc
      ]

deriving instance ( Eff (Embed IO) m, MonadCatch m
                  , Threaders '[ReaderThreads] m p
                  )
               => Carrier (SafeErrorToIOSimpleC e m)


newtype SafeErrorToErrorIOSimpleC exc m a = SafeErrorToErrorIOSimpleC {
    unSafeErrorToErrorIOSimpleC ::
        IntroUnderC (SafeError exc) '[Catch exc, Throw exc]
      ( SafeErrorToErrorC exc
      ( InterpretErrorSimpleC exc
      ( m
      ))) a
  } deriving ( Functor, Applicative, Monad
             , Alternative, MonadPlus
             , MonadFix, MonadFail, MonadIO
             , MonadThrow, MonadCatch, MonadMask
             , MonadBase b, MonadBaseControl b
             )
    deriving MonadTrans
    via CompositionBaseT
     '[ IntroUnderC (SafeError exc) '[Catch exc, Throw exc]
      , SafeErrorToErrorC exc
      , InterpretErrorSimpleC exc
      ]

deriving instance (Carrier m , Threaders '[ReaderThreads] m p)
               => Carrier (SafeErrorToErrorIOSimpleC e m)
