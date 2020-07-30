-- | Module exporting typical type classes that are newtype-derived by Carriers
module Control.Effect.Internal.Derive
  ( Alternative, MonadPlus
  , MonadFix, Fail.MonadFail, MonadIO
  , MonadThrow, MonadCatch, MonadMask
  , MonadBase, MonadBaseControl
  , MonadTrans, MonadTransControl
  , IdentityT
  ) where

-- TODO: Provide a TH macro to do the derivation too.
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.Catch
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
