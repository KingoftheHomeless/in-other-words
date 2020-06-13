module Control.Effect.Union where

import Data.Coerce

import Control.Effect.Internal
import Control.Effect.Internal.Utils
import Control.Effect.Internal.Union
import Control.Effect.Internal.KnownList

type UnionC b m = UnionC' b (StripPrefix b (Derivs m)) m

newtype UnionC' (l :: [Effect]) (r :: [Effect]) m a = UnionC { unUnionC :: m a }
  deriving (Functor, Applicative, Monad) via m

instance ( KnownList l
         , Carrier m
         , Derivs m ~ Append l r
         ) => Carrier (UnionC' l r m) where
  type (Derivs (UnionC' l r m)) = Union l ': r
  type (Prims (UnionC' l r m)) = Prims m

  algP = coerce (algP @m)

  reformulate (n :: forall x. UnionC' l r m x -> z x) alg =
    let
      algDerivs :: Algebra (Derivs m) z
      algDerivs = reformulate @m (n .# UnionC) alg
    in
      powerAlg (weakenAlgN (singList @l) algDerivs) $ \(Union pr e) ->
        algDerivs (Union (lengthenMembership @r pr) e)

  algD =
    let
      algD' :: Algebra (Derivs m) (UnionC' l r m)
      algD' = coerce (algD @m)
    in
      powerAlg (weakenAlgN (singList @l) algD') $ \(Union pr e) ->
        algD' (Union (lengthenMembership @r pr) e)

runUnion :: forall b m a
          . UnionC b m a
         -> m a
runUnion = unUnionC
