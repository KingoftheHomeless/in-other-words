{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Effly where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import qualified Control.Monad.Fail as Fail
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import qualified Control.Monad.Catch
import Control.Monad.Trans.Control hiding (embed)

import Control.Effect.Type.Alt
import Control.Effect.Type.ErrorIO
import Control.Effect.Type.Mask
import Control.Effect.Type.Bracket
import Control.Effect.Type.Embed
import Control.Effect.Type.Fail
import Control.Effect.Type.Fix
import Control.Effect.Internal
import Control.Effect.Internal.Utils

-- | A newtype wrapper with instances based around the effects of @m@
-- when possible; 'Effly' as in "Effectfully."
--
-- This is often useful for making use of these instances inside of
-- interpreter handlers, or within application code.
newtype Effly m a = Effly { runEffly :: m a }
  deriving ( Functor, Applicative, Monad
           -- , MonadThrow, MonadCatch, MonadMask -- TODO: Should we keep these?
           , MonadBase b, MonadBaseControl b
           , Carrier
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance Eff Alt m => Alternative (Effly m) where
  empty = send Empty
  {-# INLINE empty #-}

  ma <|> mb = send (Alt ma mb)
  {-# INLINE (<|>) #-}

instance Eff Alt m => MonadPlus (Effly m)

instance Eff (Embed IO) m => MonadIO (Effly m) where
  liftIO = send .# Embed
  {-# INLINE liftIO #-}

instance Eff Fix m => MonadFix (Effly m) where
  mfix = send .# Fix
  {-# INLINE mfix #-}

instance Eff Fail m => Fail.MonadFail (Effly m) where
  fail = send .# Fail
  {-# INLINE fail #-}

instance Eff ErrorIO m => MonadThrow (Effly m) where
  throwM = send . ThrowIO
  {-# INLINE throwM #-}

instance Eff ErrorIO m => MonadCatch (Effly m) where
  catch m h = send (CatchIO m h)
  {-# INLINE catch #-}

instance Effs '[Mask, Bracket, ErrorIO] m => MonadMask (Effly m) where
  mask main = send (Mask InterruptibleMask main)
  {-# INLINE mask #-}

  uninterruptibleMask main = send (Mask UninterruptibleMask main)
  {-# INLINE uninterruptibleMask #-}

  generalBracket acquire release use =
    send (GeneralBracket acquire release use)
  {-# INLINE generalBracket #-}
