{-# LANGUAGE DerivingVia #-}
module Control.Effect.Embed
  ( -- * Effect
    Embed(..)

    -- * Actions
  , embed

    -- * Interpreters
  , RunMC(RunMC)
  , runM

  , embedToEmbed

  , EmbedToMonadBaseC
  , embedToMonadBase

  , EmbedToMonadIOC
  , embedToMonadIO

    -- * Simple variants
  , embedToEmbedSimple
  ) where

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
  {-# INLINE effHandler #-}

instance (MonadIO m, Carrier m) => Handler EmbedToMonadIOH (Embed IO) m where
  effHandler = liftBase . liftIO .# unEmbed
  {-# INLINE effHandler #-}

type EmbedToMonadBaseC b = InterpretC EmbedToMonadBaseH (Embed b)
type EmbedToMonadIOC = InterpretC EmbedToMonadIOH (Embed IO)

-- | Transform an 'Embed' effect into another 'Embed' effect
-- by providing a natural transformation to convert monadic values
-- of one monad to the other.
--
-- This has a higher-rank type, as it makes use of 'InterpretReifiedC'.
-- **This makes 'embedToEmbed' very difficult to use partially applied.**
-- **In particular, it can't be composed using @'.'@.**
--
-- If performance is secondary, consider using the slower
-- 'embedtoEmbedSimple', which doesn't have a higher-rank type.
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
