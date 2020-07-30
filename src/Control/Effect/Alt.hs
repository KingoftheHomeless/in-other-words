{-# LANGUAGE BlockArguments, DerivingVia #-}
module Control.Effect.Alt
  ( -- * Effect
    Alt(..)
  , Alternative(..)

    -- * Interpretations
  , AltMaybeC
  , runAltMaybe

  , InterpretAltC(..)
  , InterpretAltReifiedC

  , altToError

  , AltToNonDetC
  , altToNonDet

    -- * Simple variants
  , InterpretAltSimpleC(..)
  , altToErrorSimple

    -- * Threading constraints
  , ErrorThreads
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
import Control.Monad.Trans.Identity

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
  {-# INLINE effHandler #-}

type AltMaybeC = CompositionC
 '[ IntroUnderC Alt '[Catch (), Throw ()]
  , InterpretAltC AltToErrorUnitH
  , ErrorC ()
  ]


-- | Run an 'Alt' effect purely, returning @Nothing@ on an unhandled
-- 'empty'.
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
  .# runComposition
{-# INLINE runAltMaybe #-}

-- | Transform an 'Alt' effect into 'Error' by describing it in
-- terms of 'throw' and 'catch', using the provided exception to act as 'empty'.
--
-- This has a higher-rank type, as it makes use of 'InterpretAltReifiedC'.
-- **This makes 'failToThrow' very difficult to use partially applied.**
-- **In particular, it can't be composed using @'.'@.**
--
-- If performance is secondary, consider using the slower 'altToErrorSimple',
-- which doesn't have a higher-rank type.
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
  {-# INLINE effHandler #-}


type AltToNonDetC = InterpretAltC AltToNonDetH

-- | Transform an 'Alt' effect into 'NonDet' by describing it
-- in terms of 'lose' and 'choose'.
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
