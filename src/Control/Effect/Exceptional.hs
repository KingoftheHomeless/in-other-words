{-# LANGUAGE DerivingVia #-}
module Control.Effect.Exceptional
  ( -- * Effect
    Exceptional

    -- * Actions
  , ExceptionallyC
  , catching
  , trying
  , throwing

    -- * Interpretations
  , ExceptionalC
  , runExceptional

  , runExceptionalJust

    -- * Simple variants of interpretations
  , runExceptionalJustSimple
  ) where

import Data.Coerce

import Control.Effect
import Control.Effect.Error
import Control.Effect.Union

import Control.Effect.Carrier

import Control.Effect.Internal.Utils
import Control.Monad.Trans.Identity


-- | An effect that allows for the safe use of an effect @eff@ that may
-- throw exceptions of the type @exc@ by forcing the user to eventually
-- catch those exceptions at some point of the program.
--
-- The main combinator of 'Exceptional' is 'catching'.
newtype Exceptional eff exc m a = Exceptional (Union '[eff, Catch exc] m a)


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
  type Derivs (ExceptionallyC eff exc m) = eff ': Derivs m
  type Prims  (ExceptionallyC eff exc m) = Prims m

  algPrims = coerce (algPrims @m)
  {-# INLINE algPrims #-}

  reformulate n alg = powerAlg' (reformulate @m (n .# lift) alg) $ \e ->
    reformulate (n .# lift) alg $ inj $ Exceptional @eff @exc (Union Here e)
  {-# INLINE reformulate #-}

  algDerivs = powerAlg' (coerce (algDerivs @m)) $ \e ->
    coerceAlg (algDerivs @m) $ inj $ Exceptional @eff @exc (Union Here e)
  {-# INLINE algDerivs#-}

-- | Gain access to @eff@ within a region, but only if you're ready
-- to handle any exception @e :: exc@ that may arise from the use
-- of @eff@ within that region.
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

-- | Gain access to @eff@ within a region. If any use of @eff@
-- within that region throws an exception @e :: exc@, then
-- then this returns @Left e@.
trying :: forall eff exc m a
        . Eff (Exceptional eff exc) m
       => ExceptionallyC eff exc m a
       -> m (Either exc a)
trying m = fmap Right m `catching` (return . Left)
{-# INLINE trying #-}

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
