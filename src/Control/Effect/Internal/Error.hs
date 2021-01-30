{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE BlockArguments, DerivingVia #-}
module Control.Effect.Internal.Error where

import Data.Coerce

import Control.Applicative
import Control.Monad

import Control.Effect
import Control.Effect.ErrorIO
import Control.Effect.Type.Throw
import Control.Effect.Type.Catch
import Control.Effect.Optional

import Control.Effect.Carrier

import Control.Monad.Trans.Except

import qualified Control.Exception as X
import qualified Control.Monad.Catch as C

-- For coercion purposes
import Control.Effect.Carrier.Internal.Interpret
import Control.Effect.Carrier.Internal.Intro
import Control.Effect.Carrier.Internal.Compose
import Control.Effect.Internal.ErrorIO
import Control.Effect.Internal.Utils

-- For errorToIO
import Data.Unique
import GHC.Exts (Any)
import Unsafe.Coerce

newtype ThrowC e m a = ThrowC { unThrowC :: ExceptT e m a }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl)

newtype ErrorC e m a = ErrorC { unErrorC :: ExceptT e m a }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl)

instance ( Carrier m
         , Threads (ExceptT e) (Prims m)
         )
      => Carrier (ThrowC e m) where
  type Derivs (ThrowC e m) = Throw e ': Derivs m
  type Prims  (ThrowC e m) = Prims m

  algPrims = coerce (thread @(ExceptT e) (algPrims @m))
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n . lift) alg) $ \case
    Throw e -> n (ThrowC (throwE e))
  {-# INLINEABLE reformulate #-}


instance ( Carrier m
         , Threads (ExceptT e) (Prims m)
         )
      => Carrier (ErrorC e m) where
  type Derivs (ErrorC e m) = Catch e ': Throw e ': Derivs m
  type Prims  (ErrorC e m) = Optional ((->) e) ': Prims m

  algPrims = powerAlg (coerce (algPrims @(ThrowC e m))) $ \case
    Optionally h m -> ErrorC (unErrorC m `catchE` (return . h))
  {-# INLINEABLE algPrims #-}

  reformulate n alg =
    powerAlg (
      coerceReform (reformulate @(ThrowC e m)) n (weakenAlg alg)
    ) $ \case
      Catch m h -> join $ (alg . inj) $ Optionally h (fmap pure m)
  {-# INLINEABLE reformulate #-}


-- | 'ErrorThreads' accepts the following primitive effects:
--
-- * 'Control.Effect.Regional.Regional' @s@
-- * 'Control.Effect.Optional.Optional' @s@ (when @s@ is a functor)
-- * 'Control.Effect.BaseControl.BaseControl' @b@
-- * 'Control.Effect.Type.Unravel.Unravel' @p@
-- * 'Control.Effect.Type.ListenPrim.ListenPrim' @o@ (when @o@ is a 'Monoid')
-- * 'Control.Effect.Type.WriterPrim.WriterPrim' @o@ (when @o@ is a 'Monoid')
-- * 'Control.Effect.Type.ReaderPrim.ReaderPrim' @i@
-- * 'Control.Effect.Mask.Mask'
-- * 'Control.Effect.Bracket.Bracket'
-- * 'Control.Effect.Fix.Fix'
class    ( forall e. Threads (ExceptT e) p
         ) => ErrorThreads p
instance ( forall e. Threads (ExceptT e) p
         ) => ErrorThreads p

type ReifiesErrorHandler s s' e m =
  ( ReifiesHandler s (Catch e) (InterpretC (ViaReifiedH s') (Throw e) m)
  , ReifiesHandler s' (Throw e) m
  )


newtype InterpretErrorC' s s' e m a = InterpretErrorC' {
    unInterpretErrorC' ::
        InterpretC (ViaReifiedH s)  (Catch e)
      ( InterpretC (ViaReifiedH s') (Throw e)
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
     '[ InterpretC (ViaReifiedH s)  (Catch e)
      , InterpretC (ViaReifiedH s') (Throw e)
      ]

deriving instance (Carrier m, ReifiesErrorHandler s s' e m)
               => Carrier (InterpretErrorC' s s' e m)

type InterpretErrorC e m a =
     forall s s'
   . ReifiesErrorHandler s s' e m
  => InterpretErrorC' s s' e m a


newtype ErrorToIOC' s s' e m a = ErrorToIOC' {
    unErrorToIOC' ::
        IntroC '[Catch e, Throw e] '[ErrorIO]
      ( InterpretErrorC' s s' e
      ( ErrorIOToIOC
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
     '[ IntroC '[Catch e, Throw e] '[ErrorIO]
      , InterpretErrorC' s s' e
      , ErrorIOToIOC
      ]

deriving instance ( Carrier m, C.MonadCatch m
                  , ReifiesErrorHandler s s' e (ErrorIOToIOC m)
                  )
               => Carrier (ErrorToIOC' s s' e m)

type ErrorToIOC e m a =
     forall s s'
   . ReifiesErrorHandler s s' e (ErrorIOToIOC m)
  => ErrorToIOC' s s' e m a

-- KingoftheHomeless: We could skip having to use 'OpaqueExc'
-- by requiring the exception type @e@ to be typeable. Or have it be
-- an instance of 'Exception'.
--
-- I choose not to for two reasons:
--   1. By making use of OpaqueExc and checking unique references,
--      we guarantee that exceptions belonging to an @'Error' e@ effect
--      interpreted with 'errorToErrorIO' won't get caught by 'catch'es
--      belonging to /another/, identical @'Error' e@ effect interpreted
--      using 'errorToErrorIO'. So by using OpaqueExc, we get coherency.
--
--  2. In case we eventually implement a system for polymorphic effect
--     interpreters inside of application code, like something like this:
--    @
--    manageError :: HasErrorInterpreter s m
--                => ProvidedErrorInterpreterC s e m a
--                -> m (Either e a)
--    @
--    of which 'errorToErrorIO' should be a valid implementation, then
--    we shouldn't place any constraints upon @e@.
data OpaqueExc = OpaqueExc Unique Any

instance Show OpaqueExc where
  showsPrec _ (OpaqueExc uniq _) =
      showString "errorToIO/errorToErrorIO: Escaped opaque exception. \
                 \Unique hash is: " . shows (hashUnique uniq) . showString ". \
                 \This should only happen if the computation that threw the \
                 \exception was somehow invoked outside of the argument of \
                 \'errorToIO'; for example, if you 'async' an exceptional \
                 \computation inside of the argument provided to 'errorToIO', \
                 \and then 'await' on it *outside* of the argument provided to \
                 \'errorToIO'. \
                 \If that or any similar shenanigans seems unlikely, then \
                 \please open an issue on the GitHub repository."

instance X.Exception OpaqueExc

-- | Runs connected 'Throw' and 'Catch' effects -- i.e. 'Error' --
-- by transforming them into 'ErrorIO' and @'Embed' IO@
--
-- This has a higher-rank type, as it makes use of 'InterpretErrorC'.
-- __This makes 'errorToErrorIO' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'errorToErrorIOSimple', which doesn't have a higher-rank type.
errorToErrorIO :: forall e m a
                . Effs '[ErrorIO, Embed IO] m
               => InterpretErrorC e m a
               -> m (Either e a)
errorToErrorIO main = do
  !uniq <- embed newUnique
  let
    main' =
        interpret \case
          Throw e -> throwIO (OpaqueExc uniq (unsafeCoerce e))
      $ interpret \case
          Catch m h -> m `catchIO` \exc@(OpaqueExc uniq' e) ->
            if uniq == uniq' then
              h (unsafeCoerce e)
            else
              throwIO exc
      $ unInterpretErrorC'
      $ main
  fmap Right main' `catchIO` \exc@(OpaqueExc uniq' e) ->
    if uniq == uniq' then
      return $ Left (unsafeCoerce e)
    else
      throwIO exc

data ErrorToErrorIOAsExcH

instance (Eff ErrorIO m, Exception e)
      => Handler ErrorToErrorIOAsExcH (Throw e) m where
  effHandler (Throw e) = throwIO e


instance (Eff ErrorIO m, Exception e)
      => Handler ErrorToErrorIOAsExcH (Catch e) m where
  effHandler (Catch m f) =  m `catchIO` f

newtype ErrorToErrorIOAsExcC e m a = ErrorToErrorIOAsExcC  {
    unErrorToErrorIOAsExcC ::
        InterpretC ErrorToErrorIOAsExcH (Catch e)
      ( InterpretC ErrorToErrorIOAsExcH (Throw e)
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
     '[ InterpretC ErrorToErrorIOAsExcH  (Catch e)
      , InterpretC ErrorToErrorIOAsExcH (Throw e)
      ]

deriving instance (Eff ErrorIO m, Exception e)
               => Carrier (ErrorToErrorIOAsExcC e m)

-- | Runs connected 'Throw' and 'Catch' effects -- i.e. 'Error' --
-- by transforming them into 'ErrorIO'.
--
-- Unlike 'errorToErrorIO', values of @e@ are thrown and caught directly as 'IO'
-- exceptions. This means that, for example, 'catchIO' is able to catch
-- exceptions of @e@ that you throw with 'throw', -and 'catch' is able to catch
-- exceptions of type @e@ that are thrown with 'throwIO', or by 'embed'ded 'IO'
-- actions.
--
-- @'Derivs' ('ErrorToErrorIOAsExcC' e m) = 'Catch' e ': 'Throw' e ': 'Derivs' m@
--
-- @'Control.Effect.Primitive.Prims' ('ErrorToErrorIOAsExcC' e m) = 'Control.Effect.Primitive.Prims' m@
errorToErrorIOAsExc
  :: (Exception e, Eff ErrorIO m)
  => ErrorToErrorIOAsExcC e m a
  -> m a
errorToErrorIOAsExc =
     interpretViaHandler
  .# interpretViaHandler
  .# unErrorToErrorIOAsExcC
{-# INLINE errorToErrorIOAsExc #-}

-- | Runs connected 'Throw' and 'Catch' effects -- i.e. 'Error' --
-- by making use of 'IO' exceptions.
--
-- @'Derivs' ('ErrorToIOC' e m) = 'Catch' e ': 'Throw' e ': 'Derivs' m@
--
-- @'Control.Effect.Primitive.Prims'  ('ErrorToIOC' e m) = 'Control.Effect.Optional.Optional' ((->) 'Control.Exception.SomeException') ': 'Control.Effect.Primitive.Prims' m@
--
-- This has a higher-rank type, as it makes use of 'ErrorToIOC'.
-- __This makes 'errorToIO' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'errorToIOSimple', which doesn't have a higher-rank type.
errorToIO :: forall e m a
           . ( C.MonadCatch m
             , Eff (Embed IO) m
             )
          => ErrorToIOC e m a
          -> m (Either e a)
errorToIO m =
    errorIOToIO
  $ errorToErrorIO
  $ introUnderMany
  $ unErrorToIOC'
  $ m
{-# INLINE errorToIO #-}

newtype ErrorToIOAsExcC e m a = ErrorToIOAsExcC {
    unErrorToIOAsExcC ::
        IntroC '[Catch e, Throw e] '[ErrorIO]
      ( ErrorToErrorIOAsExcC e
      ( ErrorIOToIOC
        m
      )) a
  } deriving ( Functor, Applicative, Monad
             , Alternative, MonadPlus
             , MonadFix, MonadFail, MonadIO
             , MonadThrow, MonadCatch, MonadMask
             , MonadBase b, MonadBaseControl b
             )
    deriving (MonadTrans, MonadTransControl)
    via CompositionBaseT
     '[ IntroC '[Catch e, Throw e] '[ErrorIO]
      , ErrorToErrorIOAsExcC e
      , ErrorIOToIOC
      ]

deriving instance (Exception e, C.MonadCatch m, Carrier m)
               => Carrier (ErrorToIOAsExcC e m)

-- | Runs connected 'Throw' and 'Catch' effects -- i.e. 'Error' --
-- by treating values of @e@ as an 'IO' exceptions.
--
-- Unlike 'errorToIO', values of @e@ are thrown and caught directly as 'IO'
-- exceptions. This means that, for example, 'catchIO' is able to catch
-- exceptions of @e@ that you throw with 'throw', and 'catch' is able to catch
-- exceptions of type @e@ that are thrown with 'throwIO', or by 'embed'ded 'IO'
-- actions.
--
-- @'Derivs' ('ErrorToIOAsExcC' e m) = 'Catch' e ': 'Throw' e ': 'Derivs' m@
--
-- @'Control.Effect.Primitive.Prims' ('ErrorToIOAsExcC' e m) = 'Control.Effect.Optional.Optional' ((->) 'Control.Exception.SomeException') ': 'Control.Effect.Primitive.Prims' m@
errorToIOAsExc
  :: ( Exception e
     , C.MonadCatch m
     , Carrier m
     )
  => ErrorToIOAsExcC e m a
  -> m a
errorToIOAsExc =
     errorIOToIO
  .# errorToErrorIOAsExc
  .# introUnderMany
  .# unErrorToIOAsExcC
{-# INLINE errorToIOAsExc #-}

newtype InterpretErrorSimpleC e m a = InterpretErrorSimpleC {
    unInterpretErrorSimpleC ::
        InterpretSimpleC (Catch e)
      ( InterpretSimpleC (Throw e)
      ( m
      )) a
  } deriving ( Functor, Applicative, Monad
             , Alternative, MonadPlus
             , MonadFix, MonadFail, MonadIO
             , MonadThrow, MonadCatch, MonadMask
             , MonadBase b, MonadBaseControl b
             )
    deriving MonadTrans
    via CompositionBaseT
     '[ InterpretSimpleC (Catch e)
      , InterpretSimpleC (Throw e)
      ]

deriving instance (Carrier m, Threaders '[ReaderThreads] m p)
               => Carrier (InterpretErrorSimpleC e m)

newtype ErrorToIOSimpleC e m a = ErrorToIOSimpleC {
    unErrorToIOSimpleC ::
        IntroC '[Catch e, Throw e] '[ErrorIO]
      ( InterpretErrorSimpleC e
      ( ErrorIOToIOC
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
     '[ IntroC '[Catch e, Throw e] '[ErrorIO]
      , InterpretErrorSimpleC e
      , ErrorIOToIOC
      ]

deriving instance ( Eff (Embed IO) m, C.MonadCatch m
                  , Threaders '[ReaderThreads] m p
                  )
               => Carrier (ErrorToIOSimpleC e m)


-- | Runs connected 'Throw' and 'Catch' effects -- i.e. 'Error' --
-- by transforming them into 'ErrorIO' and @'Embed' IO@
--
-- This is a less performant version of 'errorToErrorIO' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
errorToErrorIOSimple :: forall e m a p
                      . ( Effs '[ErrorIO, Embed IO] m
                        , Threaders '[ReaderThreads] m p
                        )
                     => InterpretErrorSimpleC e m a
                     -> m (Either e a)
errorToErrorIOSimple main = do
  !uniq <- embed newUnique
  let
    main' =
        interpretSimple \case
          Throw e -> throwIO (OpaqueExc uniq (unsafeCoerce e))
      $ interpretSimple \case
          Catch m h -> m `catchIO` \exc@(OpaqueExc uniq' e) ->
            if uniq == uniq' then
              h (unsafeCoerce e)
            else
              throwIO exc
      $ unInterpretErrorSimpleC
      $ main
  fmap Right main' `catchIO` \exc@(OpaqueExc uniq' e) ->
    if uniq == uniq' then
      return $ Left (unsafeCoerce e)
    else
      throwIO exc

-- | Runs connected 'Throw' and 'Catch' effects -- i.e. 'Error' --
-- by making use of 'IO' exceptions.
--
-- @'Derivs' ('ErrorToIOSimpleC' e m) = 'Catch' e ': 'Throw' e ': 'Derivs' m@
--
-- @'Control.Effect.Primitive.Prims'  ('ErrorToIOSimpleC' e m) = 'Control.Effect.Optional.Optional' ((->) 'Control.Exception.SomeException') ': 'Control.Effect.Primitive.Prims' m@
--
-- This is a less performant version of 'errorToIO' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
errorToIOSimple :: forall e m a p
                 . ( Eff (Embed IO) m
                   , MonadCatch m
                   , Threaders '[ReaderThreads] m p
                   )
                => ErrorToIOSimpleC e m a
                -> m (Either e a)
errorToIOSimple =
     errorIOToIO
  #. errorToErrorIOSimple
  .# introUnderMany
  .# unErrorToIOSimpleC
{-# INLINE errorToIOSimple #-}
