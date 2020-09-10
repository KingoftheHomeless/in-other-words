{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Unlift
 ( -- * Effects
   Unlift(..)

   -- Threading utilities
 , threadUnliftViaClass

   -- 'MonadBaseControlPure' and 'MonadTransControlPure'
 , MonadBaseControlPure
 , unliftBase

 , MonadTransControlPure
 , unliftT
 ) where

import Control.Effect.Internal.Union
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
-- non-trivial monad transformer, then you need to make a
-- a @'ThreadsEff'@ instance for that monad transformer to lift
-- @'Unlift' b@ (if possible). 'threadUnliftViaClass' can help you with that.
data Unlift b m a where
  Unlift :: ((forall x. m x -> b x) -> b a) -> Unlift b m a

-- | A valid definition of 'threadEff' for a @'ThreadsEff' ('Unlift' b) t@ instance,
-- given that @t@ is a 'MonadTransControl' where @'StT' t a ~ a@ holds for all @a@.
threadUnliftViaClass :: forall b t m a
                      . (MonadTransControlPure t, Monad m)
                     => (forall x. Unlift b m x -> m x)
                     -> Unlift b (t m) a -> t m a
threadUnliftViaClass alg (Unlift main) = unliftT $ \lowerT ->
  alg $ Unlift $ \lowerM -> main (lowerM . lowerT)
{-# INLINE threadUnliftViaClass #-}

instance ThreadsEff (ReaderT i) (Unlift b) where
  threadEff alg (Unlift main) = ReaderT $ \s ->
    alg $ Unlift $ \lower -> main (lower . (`runReaderT` s))
  {-# INLINE threadEff #-}
