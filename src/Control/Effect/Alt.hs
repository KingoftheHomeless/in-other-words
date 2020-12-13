{-# LANGUAGE BlockArguments, DerivingVia #-}
module Control.Effect.Alt
  ( -- * Effects
    Alt(..)
  , Alternative(..)

    -- * Interpretations
  , runAltMaybe

  , altToError

  , altToNonDet

    -- * Simple variants of interpretations
  , altToErrorSimple

    -- * Threading constraints
  , ErrorThreads

    -- * Carriers
  , AltMaybeC

  , InterpretAltC(..)
  , InterpretAltReifiedC

  , AltToNonDetC

  , InterpretAltSimpleC(..)
  ) where

import Control.Applicative
import Control.Monad

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Error
import Control.Effect.NonDet
import Control.Effect.Type.Alt

-- For coercion purposes
import Control.Effect.Internal.Utils
import Control.Effect.Internal.Error
import Control.Effect.Carrier.Internal.Compose
import Control.Effect.Carrier.Internal.Interpret
import Control.Effect.Carrier.Internal.Intro
import Control.Monad.Trans.Except

-- | Like InterpretC specialized to interpret 'Alt', but has 'Alternative' and
-- 'MonadPlus' instances based on the interpreted 'Alt'.
newtype InterpretAltC h m a = InterpretAltC {
    unInterpretAltC :: InterpretC h Alt m a
  }
  deriving ( Functor, Applicative, Monad
           , MonadFix, MonadIO, MonadFail
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl)

-- Like InterpretSimpleC specialized to interpret 'Alt', but has 'Alternative' and
-- 'MonadPlus' instances based on the interpreted 'Alt'.
newtype InterpretAltSimpleC m a = InterpretAltSimpleC {
    unInterpretAltSimpleC :: InterpretSimpleC Alt m a
  }
  deriving ( Functor, Applicative, Monad
           , MonadFix, MonadIO, MonadFail
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving MonadTrans

type InterpretAltReifiedC m a =
     forall s
   . ReifiesHandler s Alt m
  => InterpretAltC (ViaReifiedH s) m a

deriving newtype instance Handler h Alt m => Carrier (InterpretAltC h m)

deriving via Effly (InterpretAltC h m)
    instance Handler h Alt m => Alternative (InterpretAltC h m)

instance Handler h Alt m => MonadPlus (InterpretAltC h m)


deriving newtype instance
     (Monad m, Carrier (InterpretSimpleC Alt m))
  => Carrier (InterpretAltSimpleC m)

deriving via Effly (InterpretAltSimpleC m)
    instance (Monad m, Carrier (InterpretSimpleC Alt m))
          => Alternative (InterpretAltSimpleC m)

instance (Monad m, Carrier (InterpretSimpleC Alt m))
      => MonadPlus (InterpretAltSimpleC m)

data AltToErrorUnitH

instance Eff (Error ()) m
      => Handler AltToErrorUnitH Alt m where
  effHandler = \case
    Empty     -> throw ()
    Alt ma mb -> ma `catch` \() -> mb
  {-# INLINEABLE effHandler #-}

newtype AltMaybeC m a = AltMaybeC {
    unAltMaybeC ::
      IntroUnderC Alt '[Catch (), Throw ()]
    ( InterpretAltC AltToErrorUnitH
    ( ErrorC ()
    ( m
    ))) a
  } deriving ( Functor, Applicative, Monad
             , MonadFix, MonadFail, MonadIO
             , MonadThrow, MonadCatch, MonadMask
             , MonadBase b, MonadBaseControl b
             )
    deriving (MonadTrans, MonadTransControl)
    via CompositionBaseT
     '[ IntroUnderC Alt '[Catch (), Throw ()]
      , InterpretAltC AltToErrorUnitH
      , ErrorC ()
      ]

deriving newtype instance (Carrier m, Threads (ExceptT ()) (Prims m))
                       => Alternative (AltMaybeC m)

deriving newtype instance (Carrier m, Threads (ExceptT ()) (Prims m))
                       => MonadPlus (AltMaybeC m)

deriving newtype instance (Carrier m, Threads (ExceptT ()) (Prims m))
                       => Carrier (AltMaybeC m)

-- | Run an 'Alt' effect purely, returning @Nothing@ on an unhandled
-- 'empty'.
--
-- 'AltMaybeC' has an 'Alternative' instance based on the 'Alt'
-- effect it interprets.
--
-- @'Derivs' ('AltMaybeC' m) = 'Alt' ': 'Derivs' m@
--
-- @'Prims'  ('AltMaybeC' m) = 'Control.Effect.Optional.Optional' ((->) ()) ': 'Prims' m@
runAltMaybe :: forall m a p
             . ( Threaders '[ErrorThreads] m p
               , Carrier m
               )
            => AltMaybeC m a
            -> m (Maybe a)
runAltMaybe =
     fmap (either (const Nothing) Just)
  .# runError
  .# interpretViaHandler
  .# unInterpretAltC
  .# introUnder
  .# unAltMaybeC
{-# INLINE runAltMaybe #-}

-- | Transform an 'Alt' effect into 'Error' by describing it in
-- terms of 'throw' and 'catch', using the provided exception to act as 'empty'.
--
-- You can use this in application code to locally get access to an 'Alternative'
-- instance (since 'InterpretAltReifiedC' has an 'Alternative' instance based
-- on the 'Alt' effect this interprets).
--
-- For example:
--
-- @
-- 'altToError' ('throw' exc) 'empty' = 'throw' exc
-- @
--
-- 'altToError' has a higher-rank type, as it makes use of 'InterpretAltReifiedC'.
-- __This makes 'altToError' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower 'altToErrorSimple',
-- which doesn't have a higher-rank type. __However__, you typically don't
-- want to use 'altToErrorSimple' in application code, since 'altToErrorSimple'
-- emits a 'ReaderThreads' threading constraint (see 'Threaders').
altToError :: forall e m a
            . Eff (Error e) m
           => e
           -> InterpretAltReifiedC m a
           -> m a
altToError e m =
    interpret \case
      Empty     -> throw e
      Alt ma mb -> ma `catch` \(_ :: e) -> mb
  $ unInterpretAltC
  $ m
{-# INLINE altToError #-}

data AltToNonDetH

instance Eff NonDet m => Handler AltToNonDetH Alt m where
  effHandler = \case
    Empty     -> lose
    Alt ma mb -> choose ma mb
  {-# INLINEABLE effHandler #-}


type AltToNonDetC = InterpretAltC AltToNonDetH

-- | Transform an 'Alt' effect into 'NonDet' by describing it
-- in terms of 'lose' and 'choose'.
--
-- You can use this in application code to locally get access to an 'Alternative'
-- instance (since 'AltToNonDetC' has an 'Alternative' instance based
-- on the 'Alt' effect this interprets).
--
-- For example:
--
-- @
-- 'altToNonDet' 'empty' = 'lose'
-- @
altToNonDet :: Eff NonDet m
            => AltToNonDetC m a
            -> m a
altToNonDet = interpretViaHandler .# unInterpretAltC
{-# INLINE altToNonDet #-}


-- | Transform an 'Alt' in terms of 'throw' and 'catch', by providing an
-- exception to act as 'empty'.
--
-- This is a less performant version of 'altToError' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
--
-- Unlike 'altToError', __you typically don't want to use this in__
-- __application code__, since this emits a 'ReaderThreads'
-- threading constraint (see 'Threaders').
altToErrorSimple :: forall e m a p
                  . ( Eff (Error e) m
                    , Threaders '[ReaderThreads] m p
                    )
                 => e
                 -> InterpretAltSimpleC m a
                 -> m a
altToErrorSimple e =
     interpretSimple \case
       Empty -> throw e
       Alt ma mb -> ma `catch` \(_ :: e) -> mb
  .# unInterpretAltSimpleC
{-# INLINE altToErrorSimple #-}
