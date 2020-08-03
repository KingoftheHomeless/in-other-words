{-# LANGUAGE DerivingVia #-}
module Control.Effect.Exceptional
  ( -- * Effect
    Exceptional
  , SafeError

    -- * Actions
  , ExceptionallyC
  , catching
  , trying
  , throwing

  , catchSafe
  , trySafe

    -- * Interpretations
  , ExceptionalC
  , runExceptional

  , runExceptionalJust

  , SafeErrorToErrorC
  , safeErrorToError

  , SafeErrorC
  , runSafeError

  , SafeErrorToIOC'
  , SafeErrorToIOC
  , safeErrorToIO

  , SafeErrorToErrorIOC'
  , SafeErrorToErrorIOC
  , safeErrorToErrorIO

    -- * Simple variants of interpretations
  , runExceptionalJustSimple

  , SafeErrorToIOSimpleC
  , safeErrorToIOSimple

  , SafeErrorToErrorIOSimpleC
  , safeErrorToErrorIOSimple
  ) where

import Data.Coerce
import Data.Either

import Control.Effect
import Control.Effect.Error
import Control.Effect.ErrorIO
import Control.Effect.Union

import Control.Effect.Carrier

import Control.Effect.Internal.Utils
import Control.Monad.Trans.Identity

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
-- The main combinator of 'Exceptional' is 'catching'.
--
-- **This could be unsafe in the presence of 'Control.Effect.Conc.Conc'**.
-- If you use 'catching' on a computation that:
--
-- * Spawns an asynchronous computation
-- * Throws an exception inside the asynchronous computation from a use of @eff@
-- * Return the 'Control.Effect.Conc.Async' of that asynchronous computation
--
-- Then 'Control.Effect.Conc.wait'ing on that 'Control.Effect.Conc.Async'
-- will throw that exception without it being caught.
newtype Exceptional eff exc m a = Exceptional (Union '[eff, Catch exc] m a)

-- | A particularly useful specialization of 'Exceptional',
-- which makes 'catching' give you access to @'Error' exc@
-- inside of the region.
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

handling :: forall eff exc m a
          . ( Eff (Exceptional eff exc) m
            , RepresentationalEff eff
            )
         => ExceptionallyC exc eff m a
         -> (exc -> m a)
         -> m a
handling m h =
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
  type Derivs (ExceptionallyC eff exc m) = eff ': Catch exc ': Derivs m
  type Prims  (ExceptionallyC eff exc m) = Prims m

  algPrims = coerce (algPrims @m)
  {-# INLINE algPrims #-}

  reformulate n alg =
    powerAlg' (
    powerAlg (
      reformulate (n .# lift) alg
    ) $ \e ->
      reformulate (n .# lift) alg $ inj $
        Exceptional @eff @exc (Union (There Here) e)
    ) $ \e ->
      reformulate (n .# lift) alg $ inj $
        Exceptional @eff @exc (Union Here e)
  {-# INLINE reformulate #-}

  algDerivs =
    powerAlg' (
    powerAlg (
      coerce (algDerivs @m)
    ) $ \e ->
      coerceAlg (algDerivs @m) $ inj $ Exceptional @eff @exc (Union (There Here) e)
    ) $ \e ->
      coerceAlg (algDerivs @m) $ inj $ Exceptional @eff @exc (Union Here e)
  {-# INLINE algDerivs #-}

-- | Gain access to @eff@ and @'Catch' exc@ within a region,
-- but only if you're ready to handle any unhandld exception @e :: exc@
-- that may arise from the use of @eff@ within that region.
--
-- For example:
--
-- @
-- -- A part of the program unknowing and uncaring that the use of SomeEffect
-- -- may throw exceptions.
-- uncaringProgram :: Eff SomeEffect m => m String
-- uncaringProgram = do
--   doSomeThing
--   doSomeOtherThing
--
-- caringProgram :: Eff (Exceptionally SomeEffect SomeEffectExc) m => m String
-- caringProgram =
--   catching @eff uncaringProgram (\(exc :: SomeEffectExc) -> handleExc exc)
-- @
--
catching :: forall eff exc m a
          . Eff (Exceptional eff exc) m
         => ExceptionallyC eff exc m a
         -> (exc -> m a)
         -> m a
catching m h =
  send $ Exceptional @eff @exc $
    Union (There Here) (Catch (unExceptionallyC m) h)
{-# INLINE catching #-}

-- | Gain access to @'Error' exc@ within a region,
-- but only if you're ready to handle any unhandled exception @e :: exc@
-- that may arise from within that region.
catchSafe :: forall exc m a
           . Eff (SafeError exc) m
          => ExceptionallyC (Throw exc) exc m a
          -> (exc -> m a)
          -> m a
catchSafe = catching
{-# INLINE catchSafe #-}

-- | Gain access to @eff@ within a region. If any use of @eff@
-- within that region 'throw's an unhandled exception @e :: exc@,
-- then this returns @Left e@.
trying :: forall eff exc m a
        . Eff (Exceptional eff exc) m
       => ExceptionallyC eff exc m a
       -> m (Either exc a)
trying m = fmap Right m `catching` (return . Left)
{-# INLINE trying #-}

-- | Gain access to @'Error' exc@ within a region. If any unhandled exception
-- @e :: exc@ is 'throw'n within that region,  then this returns @Left e@.
trySafe :: forall exc m a
        . Eff (SafeError exc) m
       => ExceptionallyC (Throw exc) exc m a
       -> m (Either exc a)
trySafe = trying
{-# INLINE trySafe #-}

-- | Gain access to @eff@ within a region, rethrowing
-- any exception @e :: exc@ that may occur from the use of
-- @eff@ within that region.
throwing :: forall eff exc m a
          . Effs [Exceptional eff exc, Throw exc] m
         => ExceptionallyC eff exc m a
         -> m a
throwing m = m `catching` throw
{-# INLINE throwing #-}

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
  {-# INLINE effHandler #-}

type ExceptionalC eff exc = InterpretC ExceptionalH (Exceptional eff exc)

type SafeErrorToErrorC exc = ExceptionalC (Throw exc) exc

-- | Run an @'Exceptional' eff exc@ effect if both @eff@ and @'Catch' exc@
-- are part of the effect stack.
--
-- In order for this to be safe, you must ensure that the @'Catch' exc@
-- catches all exceptions that arise from the use of @eff@ and that
-- only uses of @eff@ throws those exceptions.
-- Otherwise, the use of 'catching' is liable to catch
-- exceptions not arising from uses of @eff@, or fail to catch
-- exceptions that do arise from uses of @eff@.
runExceptional :: forall eff exc m a
                . ( Member eff (Derivs m)
                  , Eff (Catch exc) m
                  )
               => ExceptionalC eff exc m a
               -> m a
runExceptional = interpretViaHandler
{-# INLINE runExceptional #-}

-- | Run an @'Exceptional' eff exc@ effect if @eff@ is part of the
-- effect stack, provided a function that identifies the kind of exceptions
-- that may arise from the use of @eff@.
--
-- In order for this to be safe, you must ensure that the function
-- identifies all exceptions that arise from the use of @eff@ and that
-- only uses of @eff@ throws those exceptions.
-- Otherwise, the use of 'catching' is liable to catch
-- other exceptions not arising from uses of @eff@, or fail to catch
--
-- The type of this interpreter is higher-rank, as it makes use of
-- 'InterpretReifiedC'. **This makes 'runExceptionalJust' difficult to**
-- **use partially applied; for example, you can't compose it using @'.'@.**
-- You may prefer the simpler, but less performant, 'runExceptionalJustSimple'.
runExceptionalJust :: forall eff smallExc bigExc m a
                    . ( Member eff (Derivs m)
                      , Eff (Error bigExc) m
                      )
                   => (bigExc -> Maybe smallExc)
                   -> InterpretReifiedC (Exceptional eff smallExc) m a
                   -> m a
runExceptionalJust from = interpret $ \(Exceptional e) -> case e of
  Union Here eff       -> algDerivs (Union membership eff)
  Union (There pr) eff -> case extract (Union pr eff) of
    Catch m h -> catchJust from m h
{-# INLINE runExceptionalJust #-}

-- | Run an @'Exceptional' eff exc@ effect if @eff@ is part of the
-- effect stack, provided a function that identifies the kind of exceptions
-- that may arise from the use of @eff@.
--
-- In order for this to be safe, you must ensure that the function
-- identifies all exceptions that arise from the use of @eff@ and that
-- only uses of @eff@ throws those exceptions.
-- Otherwise, the use of 'catching' is liable to catch
-- exceptions not arising from uses of @eff@, or fail to catch
-- exceptions that do arise from uses of @eff@.
--
-- This is a less performant version of 'runExceptionalJust', but doesn't have
-- a higher-rank type. This makes 'runExceptionalJustSimple' much easier to use
-- partially applied.
runExceptionalJustSimple :: forall eff smallExc bigExc m a p
                          . ( Member eff (Derivs m)
                            , Eff (Error bigExc) m
                            , Threaders '[ReaderThreads] m p
                            )
                         => (bigExc -> Maybe smallExc)
                         -> InterpretSimpleC (Exceptional eff smallExc) m a
                         -> m a
runExceptionalJustSimple from = interpretSimple $ \(Exceptional e) -> case e of
  Union Here eff       -> algDerivs (Union membership eff)
  Union (There pr) eff -> case extract (Union pr eff) of
    Catch m h -> catchJust from m h
{-# INLINE runExceptionalJustSimple #-}

-- | Run a @'SafeError' exc@ effect by transforming it into an @'Error' exc@
-- effect.
safeErrorToError :: forall exc m a
                  . Eff (Error exc) m
                 => SafeErrorToErrorC exc m a
                 -> m a
safeErrorToError = runExceptional
{-# INLINE safeErrorToError #-}

type SafeErrorC exc = CompositionC
 '[ IntroUnderC (SafeError exc) '[Catch exc, Throw exc]
  , SafeErrorToErrorC exc
  , ErrorC exc
  ]

-- | Run a @'SafeError' e@ effect purely.
--
-- @'Derivs' ('SafeErrorC' e m) = 'SafeError' e ': 'Prims' m@
--
-- @'Prims' ('SafeErrorC' e m) = 'Optional' ((->) e) ': 'Prims' m@
runSafeError :: forall e m a p
              . ( Carrier m
                , Threaders '[ErrorThreads] m p
                )
             => SafeErrorC e m a
             -> m a
runSafeError =
     fmap (fromRight bombPure)
  .# runError
  .# safeErrorToError
  .# introUnder
  .# runComposition
{-# INLINE runSafeError #-}

bombPure :: a
bombPure = errorWithoutStackTrace
  "runSafeError: Escaped exception! Unless you've imported some internal \
  \modules and did something REALLY stupid, this is a bug. Make an issue about \
  \it on the GitHub repository for in-other-words."

bombIO :: String -> a
bombIO str = errorWithoutStackTrace $
  str ++ ": Escaped exception! This is likely because an `async`ed exceptional\
  \computation escaped a `catching` through an `Async`. See \
  \Control.Effect.Exceptional.Exceptional. If that sounds unlikely, and you \
  \haven't imported any internal modules and do something really stupid, \
  \then this could be a bug. If so, make an issue about \
  \it on the GitHub repository for in-other-words."


type SafeErrorToIOC' s s' exc = CompositionC
  '[ IntroUnderC (SafeError exc) '[Catch exc, Throw exc]
   , SafeErrorToErrorC exc
   , ErrorToIOC' s s' exc
   ]

type SafeErrorToIOC e m a =
     forall s s'
   . ReifiesErrorHandler s s' e (ErrorIOToIOC m)
  => SafeErrorToIOC' s s' e m a

-- | Runs a @'SafeError' e@ effect by making use of 'IO' exceptions.
--
-- @'Derivs' ('SafeErrorToIOC' e m) = 'SafeError' e ': 'Derivs' m@
--
-- @'Prims' ('SafeErrorToIOC' e m) = 'Optional' ((->) SomeException) ': 'Prims' m@
--
-- This has a higher-rank type, as it makes use of 'SafeErrorToIOC'.
-- **This makes 'safeErrorToIO' very difficult to use partially applied.**
-- **In particular, it can't be composed using @'.'@.**
--
-- If performance is secondary, consider using the slower
-- 'safeErrorToIOSimple', which doesn't have a higher-rank type.
safeErrorToIO :: forall e m a
               . ( Eff (Embed IO) m
                 , MonadCatch m
                 )
              => SafeErrorToIOC e m a
              -> m a
safeErrorToIO m =
    fmap (fromRight (bombIO "safeErrorToIO"))
  $ errorToIO
  $ safeErrorToError
  $ introUnder
  $ runComposition
  $ m
{-# INLINE safeErrorToIO #-}

type SafeErrorToErrorIOC' s s' exc = CompositionC
  '[ IntroUnderC (SafeError exc) '[Catch exc, Throw exc]
   , SafeErrorToErrorC exc
   , InterpretErrorC' s s' exc
   ]

type SafeErrorToErrorIOC e m a =
     forall s s'
   . ReifiesErrorHandler s s' e m
  => SafeErrorToErrorIOC' s s' e m a

-- | Runs a @'SafeError' e@ effect by transforming it into 'ErrorIO'
-- and @'Embed' IO@.
--
-- This has a higher-rank type, as it makes use of 'SafeErrorToErrorIOC'.
-- **This makes 'safeErrorToErrorIO' very difficult to use partially applied.**
-- **In particular, it can't be composed using @'.'@.**
--
-- If performance is secondary, consider using the slower
-- 'safeErrorToErrorIOSimple', which doesn't have a higher-rank type.
safeErrorToErrorIO :: forall e m a
                    . Effs '[Embed IO, ErrorIO] m
                   => SafeErrorToErrorIOC e m a
                   -> m a
safeErrorToErrorIO m =
    fmap (fromRight (bombIO "safeErrorToErrorIO"))
  $ errorToErrorIO
  $ safeErrorToError
  $ introUnder
  $ runComposition
  $ m
{-# INLINE safeErrorToErrorIO #-}

type SafeErrorToIOSimpleC exc = CompositionC
  '[ IntroUnderC (SafeError exc) '[Catch exc, Throw exc]
   , SafeErrorToErrorC exc
   , ErrorToIOSimpleC exc
   ]

-- | Runs a @'SafeError' e@ effect by making use of 'IO' exceptions.
--
-- @'Derivs' ('SafeErrorToIOSimpleC' e m) = 'SafeError' e ': 'Derivs' m@
--
-- @'Prims' ('SafeErrorToIOSimpleC' e m) = 'Optional' ((->) SomeException) ': 'Prims' m@
--
-- This is a less performant version of 'safeErrorToIO' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
safeErrorToIOSimple :: forall e m a p
                     . ( Eff (Embed IO) m
                       , MonadCatch m
                       , Threaders '[ReaderThreads] m p
                       )
                    => SafeErrorToIOSimpleC e m a
                    -> m a
safeErrorToIOSimple =
     fmap (fromRight (bombIO "safeErrorToIOSimple"))
  .  errorToIOSimple
  .# safeErrorToError
  .# introUnder
  .# runComposition
{-# INLINE safeErrorToIOSimple #-}

type SafeErrorToErrorIOSimpleC exc = CompositionC
  '[ IntroUnderC (SafeError exc) '[Catch exc, Throw exc]
   , SafeErrorToErrorC exc
   , InterpretErrorSimpleC exc
   ]

-- | Runs a @'SafeError' e@ effect by transforming it into 'ErrorIO'
-- and @'Embed' IO@.
--
-- This is a less performant version of 'safeErrorToErrorIO' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
safeErrorToErrorIOSimple :: forall e m a p
                          . ( Effs '[ErrorIO, Embed IO] m
                            , Threaders '[ReaderThreads] m p
                            )
                         => SafeErrorToErrorIOSimpleC e m a
                         -> m a
safeErrorToErrorIOSimple =
     fmap (fromRight (bombIO "safeErrorToErrorIOSimple"))
  .  errorToErrorIOSimple
  .# safeErrorToError
  .# introUnder
  .# runComposition
{-# INLINE safeErrorToErrorIOSimple #-}
