{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.KnownList where

import Data.Bifunctor
import Control.Effect.Internal.Membership
import Control.Effect.Internal.Union


-- | Remove the prefix @xs@ from the list @r@.
--
-- 'Control.Effect.IntroC', 'Control.Effect.ReinterpretC' and friends don't
-- as much introduce effects as they /hide/ them, through removing effects from
-- the derived effects that the transformed carrier carries.
-- This is done thorugh 'StripPrefix'.
--
-- For example:
--
-- @
--    'Derivs' ('Control.Effect.ReinterpretSimpleC' e '[newE, newE2])
--  = e ': 'StripPrefix' '[newE, newE2] ('Derivs' m)
-- @
--
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


extendMembership :: forall r e l. SList l -> ElemOf e r -> ElemOf e (Append l r)
extendMembership SEnd pr = pr
extendMembership (SCons l) pr = There (extendMembership l pr)


splitMembership :: forall r e l
                 . SList l
                -> ElemOf e (Append l r)
                -> Either (ElemOf e l) (ElemOf e r)
splitMembership SEnd pr = Right pr
splitMembership (SCons _) Here = Left Here
splitMembership (SCons sl) (There pr) = first There (splitMembership @r sl pr)

injectMembership :: forall right e left mid
                  . SList left
                 -> SList mid
                 -> ElemOf e (Append left right)
                 -> ElemOf e (Append left (Append mid right))
injectMembership SEnd sm pr = extendMembership sm pr
injectMembership (SCons _) _ Here = Here
injectMembership (SCons sl) sm (There pr) = There (injectMembership @right sl sm pr)

-- TODO(KingoftheHomeless): This is safe to unsafeCoerce, but would it wreck optimizations?
lengthenMembership :: forall r e l. ElemOf e l -> ElemOf e (Append l r)
lengthenMembership Here = Here
lengthenMembership (There xs) = There (lengthenMembership @r xs)

weakenN :: SList l -> Union r m a -> Union (Append l r) m a
weakenN sl (Union pr e) = Union (extendMembership sl pr) e
{-# INLINE weakenN #-}

weakenMid :: forall right m a left mid
           . SList left -> SList mid
          -> Union (Append left right) m a
          -> Union (Append left (Append mid right)) m a
weakenMid sl sm (Union pr e) = Union (injectMembership @right sl sm pr) e
{-# INLINE weakenMid #-}

weakenAlgN :: SList l -> Algebra (Append l r) m -> Algebra r m
weakenAlgN sl alg u = alg (weakenN sl u)
{-# INLINE weakenAlgN #-}

weakenAlgMid :: forall right m left mid
              . SList left -> SList mid
             -> Algebra (Append left (Append mid right)) m
             -> Algebra (Append left right) m
weakenAlgMid sl sm alg u = alg (weakenMid @right sl sm u)
{-# INLINE weakenAlgMid #-}
