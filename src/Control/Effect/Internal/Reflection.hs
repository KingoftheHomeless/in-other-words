{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies #-}
module Control.Effect.Internal.Reflection
  (
    Reifies (..)
  , Tagged(..)
  , unproxy
  , reify
  , reifyTagged
  ) where

import Unsafe.Coerce
import Data.Proxy

newtype Tagged s a = Tagged { getTagged :: a }

unproxy :: (Proxy s -> a) -> Tagged s a
unproxy f = Tagged (f Proxy)
{-# INLINE unproxy #-}

class Reifies s a | s -> a where
  reflect :: a

data Skolem

newtype Magic a r = Magic (Reifies Skolem a => Tagged Skolem r)


reifyTagged :: forall a r. a -> (forall (s :: *). Reifies s a => Tagged s r) -> r
reifyTagged a k = unsafeCoerce (Magic k :: Magic a r) a
{-# INLINE reifyTagged #-}

reify :: forall a r. a -> (forall (s :: *) pr. (pr ~ Proxy, Reifies s a) => pr s -> r) -> r
reify a k = reifyTagged a (unproxy k)
{-# INLINE reify #-}
