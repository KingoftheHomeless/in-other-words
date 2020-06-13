module Control.Effect.Internal.Effly where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import qualified Control.Monad.Fail as Fail
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Control hiding (embed)

import Control.Effect.Type.Alt
import Control.Effect.Type.Embed
import Control.Effect.Type.Fail
import Control.Effect.Type.Fix
import Control.Effect.Internal
import Control.Effect.Internal.Utils

-- | A newtype wrapper with instances based around the effects of 'm';
-- 'Effly' as in "Effectfully."
--
-- Certain instances - such as 'MonadBase' and 'MonadBaseControl' -
-- are still based on 'm', since these involve functional dependencies.
newtype Effly m a = Effly { runEffly :: m a }
  deriving ( Functor, Applicative, Monad
           , Carrier, MonadBase b, MonadBaseControl b
           ) via m
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance Eff Alt m => Alternative (Effly m) where
  empty = send Empty
  ma <|> mb = send (Alt ma mb)

instance Eff Alt m => MonadPlus (Effly m)

instance Eff (Embed IO) m => MonadIO (Effly m) where
  liftIO = embed

instance Eff Fix m => MonadFix (Effly m) where
  mfix = send .# Fix

instance Eff Fail m => Fail.MonadFail (Effly m) where
  fail = send .# Fail
