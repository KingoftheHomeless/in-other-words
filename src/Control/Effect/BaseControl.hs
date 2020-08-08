{-# LANGUAGE DerivingVia, MagicHash #-}
module Control.Effect.BaseControl
  ( -- * Effects
    BaseControl

    -- * Actions
  , gainBaseControl

   -- * Interpretations
  , runBaseControl

  , baseControlToFinal

   -- * Threading utilities
  , threadBaseControlViaClass

    -- * MonadBaseControl
  , MonadBaseControl(..)
  , control

    -- * Carriers
  , GainBaseControlC(..)

  , BaseControlC
  , BaseControlToFinalC
  ) where

import Data.Coerce

import Control.Monad
import Control.Effect
import Control.Effect.Carrier

import Control.Effect.Type.Internal.BaseControl
import Control.Effect.Internal.BaseControl

import Control.Effect.Internal.Utils

import Control.Monad.Trans.Identity
import Control.Monad.Trans.Control
import GHC.Exts (Proxy#, proxy#)


newtype GainBaseControlC b z m a = GainBaseControlC {
    unGainBaseControlC :: m a
  }
  deriving ( Functor, Applicative, Monad
           , Alternative, MonadPlus
           , MonadFix, MonadFail, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , Carrier
           )
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance (Monad m, MonadBase b z, Coercible z m)
      => MonadBase b (GainBaseControlC b z m) where
  liftBase = coerce #. liftBase @_ @z
  {-# INLINE liftBase #-}

instance (Monad m, MonadBaseControl b z, Coercible z m)
      => MonadBaseControl b (GainBaseControlC b z m) where
  type StM (GainBaseControlC b z m) a = StM z a

  liftBaseWith m = coerce $ liftBaseWith @_ @z $ \lower -> m (coerceTrans lower)
  {-# INLINE liftBaseWith #-}

  restoreM =
    coerce (restoreM @_ @z @a) :: forall a. StM z a -> GainBaseControlC b z m a
  {-# INLINE restoreM #-}

-- | Locally gain access to a @'MonadBaseControl' b@ instance
-- within a region.
--
-- You'll need to use 'lift' if you want to use the 'MonadBaseControl' instance
-- with computations outside of the region.
-- This is common with effect handlers. For example:
--
-- @
-- import System.IO (FilePath, IOMode, Handle)
-- import qualified System.IO as SysIO
--
-- data WithFile m a where
--   WithFile :: FilePath -> IOMode -> (Handle -> m a) -> WithFile m a
--
-- runWithFile :: 'Eff' ('BaseControl' IO) m => 'SimpleInterpreterFor' WithFile m
-- runWithFile = 'interpretSimple' $ \case
--   WithFile fp mode c -> 'gainBaseControl' $ 'control' $ \lower ->
--     SysIO.withFile fp mode (lower . lift . c)
-- @
--
gainBaseControl
  :: forall b m a
   . Eff (BaseControl b) m
  => (  forall z
      . (MonadBaseControl b z, Coercible z m)
     => GainBaseControlC b z m a
     )
  -> m a
gainBaseControl main = join $ send $
  GainBaseControl @b (\(_ :: Proxy# z) -> unGainBaseControlC (main @z))
{-# INLINE gainBaseControl #-}


-- | Run a @'BaseControl' m@ effect, where the base @m@ is the current monad.
--
-- @'Derivs' ('BaseControlC' m) = 'BaseControl' m ': 'Derivs' m@
--
-- @'Prims'  ('BaseControlC' m) = 'BaseControl' m ': 'Prims' m@
runBaseControl :: Carrier m => BaseControlC m a -> m a
runBaseControl = unBaseControlC
{-# INLINE runBaseControl #-}

data BaseControlToFinalH
type BaseControlToFinalC b = InterpretPrimC BaseControlToFinalH (BaseControl b)

instance ( MonadBaseControl b m
         , Carrier m
         )
      => PrimHandler BaseControlToFinalH (BaseControl b) m where
  effPrimHandler (GainBaseControl main) = return $ main (proxy# :: Proxy# m)
  {-# INLINE effPrimHandler #-}

-- | Run a @'BaseControl' b@ effect, where the base @b@ is the final base monad.
--
-- @'Derivs' ('BaseControlToFinalC' b m) = 'BaseControl' b ': 'Derivs' m@
--
-- @'Prims'  ('BaseControlToFinalC' b m) = 'BaseControl' b ': 'Prims' m@
baseControlToFinal :: (MonadBaseControl b m, Carrier m) => BaseControlToFinalC b m a -> m a
baseControlToFinal = interpretPrimViaHandler
{-# INLINE baseControlToFinal #-}
