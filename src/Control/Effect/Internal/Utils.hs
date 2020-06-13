{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Effect.Internal.Utils where

import Data.Coerce
import Data.Proxy


infixr 9 #.
( #. ) :: Coercible c b => (b -> c) -> (a -> b) -> (a -> c)
( #. ) _ = coerce

infixl 8 .#
( .# ) :: Coercible b a => (b -> c) -> (a -> b) -> (a -> c)
( .# ) pbc _ = coerce pbc
