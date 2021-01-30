{-# LANGUAGE DerivingVia #-}
module Control.Effect.Exceptional
  ( -- * Effects
    Exceptional
  , SafeError

    -- * Actions
  , catching
  , trying
  , throwing

  , catchSafe
  , trySafe

    -- * Interpretations
  , runExceptional

  , runExceptionalJust

  , safeErrorToError

  , runSafeError

  , safeErrorToIO

  , safeErrorToErrorIO

    -- * Simple variants of interpretations
  , runExceptionalJustSimple

  , safeErrorToIOSimple

  , safeErrorToErrorIOSimple

    -- * Threading constraints
  , ErrorThreads

    -- * MonadCatch
  , MonadCatch

    -- * Carriers
  , ExceptionallyC
  , ExceptionalC
  , SafeErrorToErrorC
  , SafeErrorC
  , SafeErrorToIOC'
  , SafeErrorToIOC
  , SafeErrorToErrorIOC'
  , SafeErrorToErrorIOC
  , SafeErrorToIOSimpleC
  , SafeErrorToErrorIOSimpleC
  ) where

import Control.Effect
import Control.Effect.Error
import Control.Effect.ErrorIO
import Control.Effect.Union

import Control.Effect.Carrier

import Control.Effect.Internal.Exceptional
import Control.Effect.Internal.Utils

-- For coercion purposes
import Control.Monad.Trans.Except
import Control.Effect.Internal.Error
import Control.Effect.Carrier.Internal.Interpret
import Control.Effect.Carrier.Internal.Intro

-- | Gain access to @eff@ and @'Catch' exc@ within a region,
-- but only if you're ready to handle any unhandled exception @e :: exc@
-- that may arise from the use of @eff@ within that region.
--
-- For example:
--
-- @
-- -- A part of the program unknowing and uncaring that the use of SomeEffect
-- -- may throw exceptions.
-- uncaringProgram :: 'Eff' SomeEffect m => m String
-- uncaringProgram = do
--   doSomeThing
--   doSomeOtherThing
--
-- caringProgram :: 'Eff' ('Exceptional' SomeEffect SomeEffectExc) m => m String
-- caringProgram =
--   'catching' \@SomeEffect uncaringProgram (\\(exc :: SomeEffectExc) -> handlerForSomeEffectExc exc)
-- @
--
-- It's possible the @'Catch' exc@ effect @'catching'@ gives you access to
-- would override another, identical @'Catch' exc@ effect that you want to use
-- inside the region. To avoid this, use 'catching' together with 'intro1':
--
-- @'catching' \@SomeEffect ('intro1' uncaringProgram) ...@
--
-- If you do this, then @'catching'@ will only introduce @eff@ to be used
-- in @uncaringProgram@, and not @'Catch' exc@.
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

-- | Gain access to @eff@ and @'Catch' exc@ within a region.
-- If any use of @eff@ within that region 'throw's an unhandled exception
-- @e :: exc@, then this returns @Left e@.
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

-- | Gain access to @eff@ and @'Catch' exc@ within a region,
-- rethrowing any exception @e :: exc@ that may occur from the use of
-- @eff@ within that region.
throwing :: forall eff exc m a
          . Effs [Exceptional eff exc, Throw exc] m
         => ExceptionallyC eff exc m a
         -> m a
throwing m = m `catching` throw
{-# INLINE throwing #-}

-- | Run an @'Exceptional' eff exc@ effect if both @eff@ and @'Catch' exc@
-- are part of the effect stack.
--
-- In order for this to be safe, you must ensure that the @'Catch' exc@
-- catches all exceptions that arise from the use of @eff@ and that
-- only uses of @eff@ throws those exceptions.
-- Otherwise, the use of 'catching' is liable to catch exceptions
-- not arising from uses of @eff@, or fail to catch
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
-- exceptions that do arise from uses of @eff@.
--
-- The type of this interpreter is higher-rank, as it makes use of
-- 'InterpretReifiedC'. __This makes 'runExceptionalJust' difficult to__
-- __use partially applied; for example, you can't compose it using @'.'@.__
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

-- | Run a @'SafeError' e@ effect purely.
--
-- @'Derivs' ('SafeErrorC' e m) = 'SafeError' e ': 'Prims' m@
--
-- @'Prims' ('SafeErrorC' e m) = 'Control.Effect.Optional.Optional' ((->) e) ': 'Prims' m@
runSafeError :: forall e m a p
              . ( Carrier m
                , Threaders '[ErrorThreads] m p
                )
             => SafeErrorC e m a
             -> m a
runSafeError =
    (>>= either (\_ -> bombPure) return)
  .# runError
  .# safeErrorToError
  .# introUnder
  .# unSafeErrorC
{-# INLINE runSafeError #-}

bombPure :: a
bombPure = errorWithoutStackTrace
  "runSafeError: Escaped exception! Unless you've imported some internal \
  \modules and did something REALLY stupid, this is a bug. Make an issue about \
  \it on the GitHub repository for in-other-words."

bombIO :: String -> a
bombIO str = errorWithoutStackTrace $
  str ++ ": Escaped exception! This is likely because an `async`ed exceptional \
  \computation escaped a `catching` through an `Async`. See \
  \Control.Effect.Exceptional.Exceptional. If that sounds unlikely, and you \
  \didn't import any internal modules and do something really stupid, \
  \then this could be a bug. If so, make an issue about \
  \it on the GitHub repository for in-other-words."


-- | Runs a @'SafeError' e@ effect by making use of 'IO' exceptions.
--
-- @'Derivs' ('SafeErrorToIOC' e m) = 'SafeError' e ': 'Derivs' m@
--
-- @'Prims' ('SafeErrorToIOC' e m) = 'Control.Effect.Optional.Optional' ((->) 'Control.Exception.SomeException') ': 'Prims' m@
--
-- This has a higher-rank type, as it makes use of 'SafeErrorToIOC'.
-- __This makes 'safeErrorToIO' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
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
    (>>= either (\_ -> bombIO "safeErrorToIO") return)
  $ errorToIO
  $ safeErrorToError
  $ introUnder
  $ unSafeErrorToIOC'
  $ m
{-# INLINE safeErrorToIO #-}


-- | Runs a @'SafeError' e@ effect by transforming it into 'ErrorIO'
-- and @'Embed' IO@.
--
-- This has a higher-rank type, as it makes use of 'SafeErrorToErrorIOC'.
-- __This makes 'safeErrorToErrorIO' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'safeErrorToErrorIOSimple', which doesn't have a higher-rank type.
safeErrorToErrorIO :: forall e m a
                    . Effs '[Embed IO, ErrorIO] m
                   => SafeErrorToErrorIOC e m a
                   -> m a
safeErrorToErrorIO m =
    (>>= either (\_ -> bombIO "safeErrorToErrorIO") return)
  $ errorToErrorIO
  $ safeErrorToError
  $ introUnder
  $ unSafeErrorToErrorIOC'
  $ m
{-# INLINE safeErrorToErrorIO #-}

-- | Runs a @'SafeError' e@ effect by making use of 'IO' exceptions.
--
-- @'Derivs' ('SafeErrorToIOSimpleC' e m) = 'SafeError' e ': 'Derivs' m@
--
-- @'Prims' ('SafeErrorToIOSimpleC' e m) = 'Control.Effect.Optional.Optional' ((->) 'Control.Exception.SomeException') ': 'Prims' m@
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
     (>>= either (\_ -> bombIO "safeErrorToIOSimple") return)
  .  errorToIOSimple
  .# safeErrorToError
  .# introUnder
  .# unSafeErrorToIOSimpleC
{-# INLINE safeErrorToIOSimple #-}

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
     (>>= either (\_ -> bombIO "safeErrorToErrorIOSimple") return)
  .  errorToErrorIOSimple
  .# safeErrorToError
  .# introUnder
  .# unSafeErrorToErrorIOSimpleC
{-# INLINE safeErrorToErrorIOSimple #-}
