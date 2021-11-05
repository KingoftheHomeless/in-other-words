{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Unlift
 ( -- * Effects
   Unlift(..)

   -- Threading utilities
 , threadUnliftViaClass
 , threadBaseControlViaUnlift

   -- 'MonadBaseControlPure' and 'MonadTransControlPure'
 , MonadBaseControlPure
 , unliftBase

 , MonadTransControlPure
 , unliftT
 ) where

import GHC.Exts (Proxy#, proxy#)
import Data.Coerce
import Control.Effect.Internal.Union
import Control.Effect.Internal.Utils
import Control.Effect.Type.Internal.BaseControl
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader

class    a ~ StM m a => Pure m a
instance a ~ StM m a => Pure m a

-- | A constraint synonym for @'MonadBaseControl' b m@ together
-- with that @StM m a ~ a@ for all @a@.
class    ( MonadBaseControl b m
         , forall x. Pure m x
         )
        => MonadBaseControlPure b m

instance ( MonadBaseControl b m
         , forall x. Pure m x
         )
        => MonadBaseControlPure b m

class    a ~ StT t a => PureT t a
instance a ~ StT t a => PureT t a

class    ( MonadTransControl t
         , forall x. PureT t x
         )
        => MonadTransControlPure t

instance ( MonadTransControl t
         , forall x. PureT t x
         )
        => MonadTransControlPure t

unliftBase :: forall b m a
            . MonadBaseControlPure b m
           => ((forall x. m x -> b x) -> b a)
           -> m a
unliftBase main = liftBaseWith $ \lower ->
  main (lower :: Pure m x => m x -> b x)
{-# INLINE unliftBase #-}

unliftT :: forall t m a
         . (MonadTransControlPure t, Monad m)
        => ((forall n x. Monad n => t n x -> n x) -> m a)
        -> t m a
unliftT main = liftWith $ \lower ->
  main (lower :: (PureT t x, Monad n) => t n x -> n x)
{-# INLINE unliftT #-}

-- | A /helper primitive effect/ for unlifting to a base monad.
--
-- Helper primitive effects are effects that allow you to avoid interpreting one
-- of your own effects as a primitive if the power needed from direct access to
-- the underlying monad can instead be provided by the relevant helper primitive
-- effect. The reason why you'd want to do this is that helper primitive effects
-- already have 'ThreadsEff' instances defined for them, so you don't have to
-- define any for your own effect.
--
-- The helper primitive effects offered in this library are -- in order of
-- ascending power -- 'Control.Effect.Regional.Regional',
-- 'Control.Effect.Optional.Optional', 'Control.Effect.BaseControl.BaseControl'
-- and 'Control.Effect.Unlift.Unlift'.
--
-- __'Unlift' is typically used as a primitive effect.__
-- If you define a 'Control.Effect.Carrier' that relies on a novel
-- non-trivial monad transformer @t@, then you need to make
-- a @'ThreadsEff' t ('Unlift' b)@ instance (if possible).
-- 'threadUnliftViaClass' can help you with that.
--
-- The following threading constraints accept 'Unlift':
--
-- * 'Control.Effect.ReaderThreads'
newtype Unlift b :: Effect where
  Unlift :: forall b m a. ((forall x. m x -> b x) -> b a) -> Unlift b m a

-- | A valid definition of 'threadEff' for a @'ThreadsEff' ('Unlift' b) t@
-- instance, given that @t@ is a 'MonadTransControl' where @'StT' t a ~ a@ holds
-- for all @a@.
threadUnliftViaClass :: forall b t m a
                      . (MonadTransControlPure t, Monad m)
                     => (forall x. Unlift b m x -> m x)
                     -> Unlift b (t m) a -> t m a
threadUnliftViaClass alg (Unlift main) = unliftT $ \lowerT ->
  alg $ Unlift $ \lowerM -> main (lowerM . lowerT)
{-# INLINE threadUnliftViaClass #-}

-- | A valid definition of 'threadEff' for a @'ThreadsEff' ('BaseControl' b) t@
-- instance, given that @t@ threads @'Unlift' z@ for any 'Monad' @z@.
threadBaseControlViaUnlift :: forall b t m a
                            . ( Monad m
                              , MonadTrans t
                              , forall z. Monad z => Monad (t z)
                              ,    forall z
                                 . Coercible z m
                                => Coercible (t z) (t m)
                              , forall z. Monad z => ThreadsEff t (Unlift z)
                              )
                           => (forall x. BaseControl b m x -> m x)
                           -> BaseControl b (t m) a -> t m a
threadBaseControlViaUnlift alg (GainBaseControl main) =
    lift $ alg $ GainBaseControl $ \(_ :: Proxy# z) ->
      main (proxy# :: Proxy# (Unlifted t z))
{-# INLINE threadBaseControlViaUnlift #-}

newtype Unlifted t m a = Unlifted { unUnlifted :: t m a }
  deriving (Functor, Applicative, Monad)
  deriving MonadTrans

instance (MonadBase b m, MonadTrans t, Monad (t m))
      => MonadBase b (Unlifted t m) where
  liftBase = lift . liftBase
  {-# INLINE liftBase #-}

instance (MonadBaseControl b m, MonadTrans t, ThreadsEff t (Unlift m),
          Monad (t m))
      => MonadBaseControl b (Unlifted t m) where
  type StM (Unlifted t m) a = StM m a

  liftBaseWith main =
    Unlifted $ threadEff (\(Unlift main') -> main' id) $ Unlift $ \lower ->
      liftBaseWith $ \run_it -> main $ run_it . lower .# unUnlifted
  {-# INLINE liftBaseWith #-}

  restoreM = Unlifted #. lift . restoreM
  {-# INLINE restoreM #-}


instance ThreadsEff (ReaderT i) (Unlift b) where
  threadEff alg (Unlift main) = ReaderT $ \s ->
    alg $ Unlift $ \lower -> main (lower . (`runReaderT` s))
  {-# INLINE threadEff #-}
