module Control.Effect.Internal.Utils
  ( Type,
    module Control.Effect.Internal.Utils
  ) where

import Data.Coerce
import Data.Kind (Type)

infixr 9 #.
(#.) :: Coercible c b => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce
{-# INLINE (#.) #-}

infixl 8 .#
(.#) :: Coercible b a => (b -> c) -> (a -> b) -> (a -> c)
(.#) pbc _ = coerce pbc
{-# INLINE (.#) #-}

coerceTrans :: (Coercible m z, Coercible n y) => (m a -> n b) -> z a -> y b
coerceTrans = coerce
{-# INLINE coerceTrans #-}

coerceM :: Coercible m n => m a -> n a
coerceM = coerce
{-# INLINE coerceM #-}
