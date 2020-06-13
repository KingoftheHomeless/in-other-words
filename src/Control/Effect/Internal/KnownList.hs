{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Effect.Internal.KnownList where

import Control.Effect.Internal.Membership
import Control.Effect.Internal.Union

type family StripPrefix xs r where
  StripPrefix '[] r = r
  StripPrefix (x ': xs) (x ': r) = StripPrefix xs r

data SList l where
  SEnd  :: SList '[]
  SCons :: SList xs -> SList (x ': xs)

class KnownList l where
  singList :: SList l

instance KnownList '[] where
  singList = SEnd
  {-# INLINE singList #-}

instance KnownList xs => KnownList (x ': xs) where
  singList = SCons singList
  {-# INLINE singList #-}

type family Append l r where
  Append '[] r = r
  Append (x ': l) r = x ': (Append l r)

extendMembership :: SList l -> ElemOf e r -> ElemOf e (Append l r)
extendMembership SEnd pr = pr
extendMembership (SCons l) pr = There (extendMembership l pr)

lengthenMembership :: forall r e l. ElemOf e l -> ElemOf e (Append l r)
lengthenMembership Here = Here
lengthenMembership (There xs) = There (lengthenMembership @r xs)

weakenN :: SList l -> Union r m a -> Union (Append l r) m a
weakenN sl (Union pr e) = Union (extendMembership sl pr) e
{-# INLINE weakenN #-}

weakenAlgN :: SList l -> Algebra (Append l r) m -> Algebra r m
weakenAlgN sl alg u = alg (weakenN sl u)
{-# INLINE weakenAlgN #-}
