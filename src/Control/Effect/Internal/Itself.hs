module Control.Effect.Internal.Itself where

import Control.Monad.Base
import Control.Monad.Trans.Control

newtype Itself m a = Itself { unItself :: m a }
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadBase m (Itself m) where
  liftBase = Itself
  {-# INLINE liftBase #-}

instance Monad m => MonadBaseControl m (Itself m) where
  type StM (Itself m) a = a

  liftBaseWith m = Itself (m unItself)
  {-# INLINE liftBaseWith #-}

  restoreM = return
  {-# INLINE restoreM #-}
