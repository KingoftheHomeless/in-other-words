{-# LANGUAGE DerivingVia #-}
module Control.Effect.Embed
  ( -- * Effects
    Embed(..)

    -- * Actions
  , embed

    -- * Interpreters
  , runM
  , runEmbed

  , embedToEmbed

  , embedToMonadBase

  , embedToMonadIO

    -- * Simple variants
  , embedToEmbedSimple

    -- * Carriers
  , RunMC(RunMC)
  , EmbedC(EmbedC)
  , EmbedToMonadBaseC
  , EmbedToMonadIOC
  ) where

import Data.Coerce
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Identity
import qualified Control.Monad.Fail as Fail
import Control.Effect.Internal
import Control.Effect.Carrier.Internal.Interpret
import Control.Effect.Type.Embed
import Control.Effect.Internal.Union
import Control.Effect.Internal.Utils
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.Trans.Control (MonadBaseControl, MonadTransControl)

embed :: Eff (Embed b) m => b a -> m a
embed = send .# Embed
{-# INLINE embed #-}

-- | The carrier for 'runM', which carries no effects but @'Embed' m@.
newtype RunMC m a = RunMC { unRunMC :: m a }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, Fail.MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance Monad m => Carrier (RunMC m) where
  type Derivs (RunMC m) = '[Embed m]
  type Prims  (RunMC m) = '[]

  algPrims = absurdU
  {-# INLINE algPrims #-}

  reformulate n _ u = n (RunMC (unEmbed (extract u)))
  {-# INLINE reformulate #-}

  algDerivs u = RunMC (unEmbed (extract u))
  {-# INLINE algDerivs #-}

newtype EmbedC m a = EmbedC { unEmbedC :: m a }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, Fail.MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , MonadBase b, MonadBaseControl b
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

-- | Run an @'Embed' m@ effect, where the embedded monad @m@ is the current monad.
--
-- Not to be confused with 'runM'. This is simply an interpreter for @'Embed' m@;
-- it doesn't extract the final monad.
--
-- @'Derivs' ('EmbedC' m) = 'Embed' m ': 'Derivs' m@
--
-- @'Prims'  ('EmbedC' m) = 'Prims' m@
runEmbed :: Carrier m => EmbedC m a -> m a
runEmbed = unEmbedC
{-# INLINE runEmbed #-}

instance Carrier m => Carrier (EmbedC m) where
  type Derivs (EmbedC m) = Embed m ': Derivs m
  type Prims  (EmbedC m) = Prims m

  algPrims = coerce (algPrims @m)
  {-# INLINEABLE algPrims #-}

  reformulate n alg = powerAlg (reformulate (n .# EmbedC) alg) (n .# EmbedC .# unEmbed)
  {-# INLINE reformulate #-}

  algDerivs = powerAlg (coerce (algDerivs @m)) (EmbedC .# unEmbed)
  {-# INLINEABLE algDerivs #-}

-- | Extract the final monad @m@ from a computation of which
-- no effects remain to be handled except for @'Embed' m@.
runM :: Monad m => RunMC m a -> m a
runM = unRunMC
{-# INLINE runM #-}

data EmbedToMonadBaseH
data EmbedToMonadIOH

instance ( MonadBase b m
         , Carrier m
         )
      => Handler EmbedToMonadBaseH (Embed b) m where
  effHandler = liftBase . liftBase .# unEmbed
  {-# INLINEABLE effHandler #-}

instance (MonadIO m, Carrier m) => Handler EmbedToMonadIOH (Embed IO) m where
  effHandler = liftBase . liftIO .# unEmbed
  {-# INLINEABLE effHandler #-}

type EmbedToMonadBaseC b = InterpretC EmbedToMonadBaseH (Embed b)
type EmbedToMonadIOC = InterpretC EmbedToMonadIOH (Embed IO)

-- | Transform an 'Embed' effect into another 'Embed' effect
-- by providing a natural transformation to convert monadic values
-- of one monad to the other.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- __This makes 'embedToEmbed' very difficult to use partially applied.__
-- __In particular, it can't be composed using @'.'@.__
--
-- If performance is secondary, consider using the slower
-- 'embedToEmbedSimple', which doesn't have a higher-rank type.
embedToEmbed :: forall b b' m a
              . Eff (Embed b') m
             => (forall x. b x -> b' x)
             -> InterpretReifiedC (Embed b) m a
             -> m a
embedToEmbed n = interpret $ \case
  Embed m -> embed (n m)
{-# INLINE embedToEmbed #-}

-- | Run an @'Embed' b@ effect if @b@ is the base of the current
-- monad @m@.
embedToMonadBase :: (MonadBase b m, Carrier m)
                 => EmbedToMonadBaseC b m a
                 -> m a
embedToMonadBase = interpretViaHandler
{-# INLINE embedToMonadBase #-}

-- | Run an @'Embed' IO@ effect if the current monad @m@ is a @MonadIO@.
embedToMonadIO :: (MonadIO m, Carrier m)
               => EmbedToMonadIOC m a
               -> m a
embedToMonadIO = interpretViaHandler
{-# INLINE embedToMonadIO #-}

-- | Transform an 'Embed' effect into another 'Embed' effect
-- by providing a natural transformation to convert monadic values
-- of one monad to the other.
--
-- This is a less performant version of 'embedToEmbed' that doesn't have
-- a higher-rank type, making it much easier to use partially applied.
embedToEmbedSimple :: forall b b' m a p
                    . ( Eff (Embed b') m
                      , Threaders '[ReaderThreads] m p
                      )
                   => (forall x. b x -> b' x)
                   -> InterpretSimpleC (Embed b) m a
                   -> m a
embedToEmbedSimple n = interpretSimple $ \case
  Embed m -> embed (n m)
{-# INLINE embedToEmbedSimple #-}
