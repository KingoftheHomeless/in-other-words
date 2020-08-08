{-# LANGUAGE BlockArguments, DerivingVia #-}
module Control.Effect.Fail
  ( -- * Effects
    Fail(..)

    -- * Interpretations
  , runFail

  , failToThrow

  , failToNonDet

  , failToAlt

    -- * Simple variants of interpretations
  , failToThrowSimple

    -- * Threading constraints
  , ErrorThreads

    -- * Carriers
  , FailC
  , InterpretFailC(..)
  , InterpretFailReifiedC
  , FailToNonDetC
  , FailToAltC
  , InterpretFailSimpleC(..)
  ) where

import Data.Coerce

import Control.Applicative
import Control.Monad

import Control.Effect
import Control.Effect.Error
import Control.Effect.NonDet
import Control.Effect.Type.Alt
import Control.Effect.Type.Fail

import Control.Effect.Carrier

-- Imports for coercion
import Control.Effect.Internal.Utils
import Control.Effect.Carrier.Internal.Interpret
import Control.Effect.Carrier.Internal.Intro
import Control.Effect.Carrier.Internal.Compose
import Control.Monad.Trans.Identity


-- | Like 'InterpretC' specialized to interpret 'Fail', but with a 'MonadFail'
-- instance based on the interpreted 'Fail'.
newtype InterpretFailC h m a = InterpretFailC {
    unInterpretFailC :: InterpretC h Fail m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

type InterpretFailReifiedC m a
   = forall s
   . ReifiesHandler s Fail m
  => InterpretFailC (ViaReifiedH s) m a

deriving via InterpretC h Fail m instance
  Handler h Fail m => Carrier (InterpretFailC h m)

deriving via Effly (InterpretFailC h m)
    instance Handler h Fail m
          => MonadFail (InterpretFailC h m)

-- | Transform a 'Fail' effect to a 'Throw' effect by providing a function
-- to transform a pattern match failure into an exception.
--
-- You can use this in application code to locally get access to a 'MonadFail'
-- instance (since 'InterpretFailReifiedC' has a 'MonadFail' instance based
-- on the 'Fail' effect this interprets).
--
-- This has a higher-rank type, as it makes use of 'InterpretFailReifiedC'.
-- __This makes 'failToThrow' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower 'failToThrowSimple',
-- which doesn't have a higher-rank type.
failToThrow :: Eff (Throw e) m
            => (String -> e)
            -> InterpretFailReifiedC m a
            -> m a
failToThrow f m =
    interpret \case
      Fail s -> throw (f s)
  $ unInterpretFailC
  $ m
{-# INLINE failToThrow #-}

data FailToAltH

type FailToAltC = InterpretFailC FailToAltH

instance Eff Alt m => Handler FailToAltH Fail m where
  effHandler _ = runEffly empty
  {-# INLINE effHandler #-}

data FailToNonDetH

instance Eff NonDet m => Handler FailToNonDetH Fail m where
  effHandler _ = lose
  {-# INLINE effHandler #-}

type FailToNonDetC = InterpretFailC FailToAltH

-- | Transform a 'Fail' effect to an 'Alt' effect by having a
-- pattern match failure be 'empty'.
--
-- You can use this in application code to locally get access to a 'MonadFail'
-- instance (since 'FailToAltC' has a 'MonadFail' instance based
-- on the 'Fail' effect this interprets).
failToAlt :: Eff Alt m
          => FailToAltC m a
          -> m a
failToAlt = interpretViaHandler .# unInterpretFailC
{-# INLINE failToAlt #-}

-- | Transform a 'Fail' effect to a 'NonDet' effect by having a
-- pattern match failure be 'lose'.
--
-- You can use this in application code to locally get access to a 'MonadFail'
-- instance (since 'FailToNonDetC' has a 'MonadFail' instance based
-- on the 'Fail' effect this interprets).
failToNonDet :: Eff Alt m
             => FailToNonDetC m a
             -> m a
failToNonDet = interpretViaHandler .# unInterpretFailC
{-# INLINE failToNonDet #-}

data FailH

type FailC = CompositionC
 '[ ReinterpretC FailH Fail '[Throw String]
  , ThrowC String
  ]

instance Eff (Throw String) m
      => Handler FailH Fail m where
  effHandler = throw @String .# coerce
  {-# INLINE effHandler #-}

-- | Run a 'Fail' effect purely, by returning @Left failureMessage@
-- upon a pattern match failure.
--
-- 'FailC' has an 'Alternative' instance based on the 'Alt'
-- effect it interprets.
runFail :: forall m a p
         . ( Threaders '[ErrorThreads] m p
           , Carrier m
           )
        => FailC m a
        -> m (Either String a)
runFail =
     runThrow
  .# reinterpretViaHandler
  .# runComposition

-- | Like 'InterpretSimpleC' specialized to interpret 'Fail', but with
-- a 'MonadFail' instance based on the interpreted 'Fail'.
newtype InterpretFailSimpleC m a = InterpretFailSimpleC {
    unInterpretFailSimpleC :: InterpretSimpleC Fail m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving MonadTrans

deriving newtype instance
     (Monad m, Carrier (InterpretSimpleC Fail m))
  => Carrier (InterpretFailSimpleC m)

instance (Monad m, Carrier (InterpretSimpleC Fail m))
       => MonadFail (InterpretFailSimpleC m) where
  fail = send .# Fail
  {-# INLINE fail #-}

-- | Transform a 'Fail' effect to a 'Throw' effect by providing a function
-- to transform a pattern match failure into an exception.
--
-- You can use this in application code to locally get access to a 'MonadFail'
-- instance (since 'InterpretFailSimpleC' has a 'MonadFail' instance based
-- on the 'Fail' effect this interprets).
--
-- This is a less performant version of 'failToThrow' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
failToThrowSimple :: forall e m a p
                   . ( Eff (Throw e) m
                     , Threaders '[ReaderThreads] m p
                     )
                  => (String -> e)
                  -> InterpretFailSimpleC m a
                  -> m a
failToThrowSimple f =
    interpretSimple \case
      Fail s -> throw (f s)
  .# unInterpretFailSimpleC
{-# INLINE failToThrowSimple #-}
