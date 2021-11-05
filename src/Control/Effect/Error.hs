{-# LANGUAGE BlockArguments #-}
module Control.Effect.Error
  ( -- * Effects
    Throw(..)
  , Catch(..)
  , Error

    -- * Actions
  , throw
  , catch
  , try
  , catchJust
  , tryJust
  , note
  , fromEither

    -- * Main Interpreters
  , runThrow

  , runError

  , errorToIO

  , errorToIOAsExc

    -- * Other interpreters
  , errorToErrorIO

  , errorToErrorIOAsExc

  , throwToThrow
  , catchToError
  , errorToError

    -- * Simple variants
  , errorToIOSimple
  , errorToErrorIOSimple

  , throwToThrowSimple
  , catchToErrorSimple
  , errorToErrorSimple

    -- * Threading constraints
  , ErrorThreads

    -- * MonadCatch
  , C.MonadCatch

    -- * Carriers
  , ThrowC
  , ErrorC
  , ErrorToIOC
  , ErrorToIOC'
  , ReifiesErrorHandler
  , InterpretErrorC
  , InterpretErrorC'
  , ErrorToIOSimpleC
  , InterpretErrorSimpleC
  ) where

import Data.Function
import Data.Coerce

import Control.Effect
import Control.Effect.Type.Throw
import Control.Effect.Type.Catch
import Control.Effect.Internal.Error

import qualified Control.Monad.Catch as C

-- For coercion purposes
import Control.Monad.Trans.Except
import Control.Effect.Carrier.Internal.Interpret
import Control.Effect.Carrier.Internal.Intro
import Control.Effect.Internal.Utils


throw :: Eff (Throw e) m => e -> m a
throw = send .# Throw
{-# INLINE throw #-}

catch :: Eff (Catch e) m => m a -> (e -> m a) -> m a
catch m h = send (Catch m h)
{-# INLINE catch #-}

try :: Eff (Catch e) m => m a -> m (Either e a)
try m = fmap Right m `catch` (return . Left)
{-# INLINE try #-}


catchJust :: forall smallExc bigExc m a
           . Eff (Error bigExc) m
          => (bigExc -> Maybe smallExc)
          -> m a
          -> (smallExc -> m a)
          -> m a
catchJust f m h = m `catch` \e -> maybe (throw e) h (f e)
{-# INLINE catchJust #-}

tryJust :: forall smallExc bigExc m a
         . Eff (Error bigExc) m
        => (bigExc -> Maybe smallExc)
        -> m a
        -> m (Either smallExc a)
tryJust f m = fmap Right m &(catchJust f)$ (return . Left)
{-# INLINE tryJust #-}

note :: Eff (Throw e) m => e -> Maybe a -> m a
note _ (Just a) = return a
note e Nothing  = throw e
{-# INLINE note #-}

fromEither :: Eff (Throw e) m => Either e a -> m a
fromEither = either throw pure
{-# INLINE fromEither #-}

-- | Run a 'Throw' effect purely.
--
-- Unlike 'runError', this does not provide the ability to catch exceptions.
-- However, it also doesn't impose any primitive effects, meaning 'runThrow' doesn't
-- restrict what interpreters are run before it.
--
-- @'Derivs' ('ThrowC' e m) = 'Throw' e ': 'Derivs' m@
--
-- @'Control.Effect.Primitive.Prims'  ('ThrowC' e m) = 'Control.Effect.Primitive.Prims' m@
runThrow :: forall e m a p
          . ( Carrier m
            , Threaders '[ErrorThreads] m p
            )
         => ThrowC e m a
         -> m (Either e a)
runThrow = coerce
{-# INLINE runThrow #-}

-- | Runs connected 'Throw' and 'Catch' effects -- i.e. 'Error' -- purely.
--
-- @'Derivs' ('ErrorC' e m) = 'Catch' e ': 'Throw' e ': 'Derivs' m@
--
-- @'Control.Effect.Primitive.Prims'  ('ErrorC' e m) = 'Control.Effect.Optional.Optional' ((->) e) ': 'Control.Effect.Primitive.Prims' m@
runError :: forall e m a p
          . ( Carrier m
            , Threaders '[ErrorThreads] m p
            )
         => ErrorC e m a
         -> m (Either e a)
runError = coerce
{-# INLINE runError #-}


-- | Transforms a @'Throw' smallExc@ effect into a @'Throw' bigExc@ effect,
-- by providing a function to convert exceptions of the smaller exception type
-- @smallExc@ to the larger exception type @bigExc@.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'throwToThrow' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'throwToThrowSimple', which doesn't have a higher-rank type.
throwToThrow :: forall smallExc bigExc m a
              . Eff (Throw bigExc) m
             => (smallExc -> bigExc)
             -> InterpretReifiedC (Throw smallExc) m a
             -> m a
throwToThrow to = interpret $ \case
  Throw e -> throw (to e)
{-# INLINE throwToThrow #-}

-- | Transforms a @'Catch' smallExc@ effect into an @'Error' bigExc@ effect, by
-- providing a function that identifies when exceptions of the larger exception type
-- @bigExc@ correspond to exceptions of the smaller exception type @smallExc@.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'catchToError' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'catchToErrorSimple', which doesn't have a higher-rank type.
catchToError :: forall smallExc bigExc m a
              . Eff (Error bigExc) m
             => (bigExc -> Maybe smallExc)
             -> InterpretReifiedC (Catch smallExc) m a
             -> m a
catchToError from = interpret $ \case
  Catch m h -> m &(catchJust from)$ h
{-# INLINE catchToError #-}



-- | Transforms connected 'Throw' and 'Catch' effects -- i.e. 'Error' --
-- into another 'Error' effect by providing functions to convert
-- between the two types of exceptions.
--
-- This has a higher-rank type, as it makes use of 'InterpretErrorC'.
-- __This makes 'errorToError' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'errorToErrorSimple', which doesn't have a higher-rank type.
errorToError :: forall smallExc bigExc m a
              . Eff (Error bigExc) m
             => (smallExc -> bigExc)
             -> (bigExc -> Maybe smallExc)
             -> InterpretErrorC smallExc m a
             -> m a
errorToError to from m0 =
    throwToThrow to
    -- can't use 'catchToError' directly,
    -- since it can't tell if it should use smallExc or bigExc for its 'Throw'.
    -- We fix that by using 'intro1'.
  $ interpret \case
      Catch m h -> m `catch` \e -> case from e of
        Just e' -> h e'
        Nothing -> intro1 $ throw e
  $ unInterpretErrorC'
  $ m0
{-# INLINE errorToError #-}

-- | Transforms a @'Throw' smallExc@ effect into a @'Throw' bigExc@ effect,
-- by providing a function to convert exceptions of the smaller exception type
-- @smallExc@ to the larger exception type @bigExc@.
--
-- This is a less performant version of 'throwToThrow' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
throwToThrowSimple :: forall smallExc bigExc m a p
                    . ( Eff (Throw bigExc) m
                      , Threaders '[ReaderThreads] m p
                      )
                   => (smallExc -> bigExc)
                   -> InterpretSimpleC (Throw smallExc) m a
                   -> m a
throwToThrowSimple to = interpretSimple $ \case
  Throw e -> throw (to e)
{-# INLINE throwToThrowSimple #-}

-- | Transforms a @'Catch' smallExc@ effect into an @'Error' bigExc@ effect, by
-- providing a function that identifies when exceptions of the larger exception type
-- @bigExc@ correspond to exceptions of the smaller exception type @smallExc@.
--
-- This is a less performant version of 'catchToError' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
catchToErrorSimple :: forall smallExc bigExc m a p
                    . ( Eff (Error bigExc) m
                      , Threaders '[ReaderThreads] m p
                      )
                   => (bigExc -> Maybe smallExc)
                   -> InterpretSimpleC (Catch smallExc) m a
                   -> m a
catchToErrorSimple from = interpretSimple $ \case
  Catch m h -> catchJust from m h
{-# INLINE catchToErrorSimple #-}

-- | Transforms connected 'Throw' and 'Catch' effects -- i.e. 'Error' --
-- into another 'Error' effect by providing functions to convert
-- between the two types of exceptions.
--
-- This is a less performant version of 'errorToError' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
errorToErrorSimple :: forall smallExc bigExc m a p
                    . ( Eff (Error bigExc) m
                      , Threaders '[ReaderThreads] m p
                      )
                   => (smallExc -> bigExc)
                   -> (bigExc -> Maybe smallExc)
                   -> InterpretErrorSimpleC smallExc m a
                   -> m a
errorToErrorSimple to from =
     throwToThrowSimple to
  .  interpretSimple \case
       Catch m h -> intro1 $ catchJust from (lift m) (lift #. h)
  .# unInterpretErrorSimpleC
{-# INLINE errorToErrorSimple #-}
