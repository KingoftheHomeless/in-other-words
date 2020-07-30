{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.Membership where

import GHC.TypeLits
import Data.Type.Equality

data ElemOf (e :: a) (r :: [a]) where
  Here  :: ElemOf e (e ': r)
  There :: ElemOf e r -> ElemOf e (_e ': r)

absurdMember :: ElemOf e '[] -> b
absurdMember = \case
{-# INLINE absurdMember #-}

deriving instance Show (ElemOf e r)

sameMember :: forall e e' r
            . ElemOf e r
           -> ElemOf e' r
           -> Maybe (e :~: e')
sameMember Here Here = Just Refl
sameMember (There pr) (There pr') = sameMember pr pr'
sameMember _ _ = Nothing

class Member e r where
  membership :: ElemOf e r

instance {-# OVERLAPPING #-} Member e (e ': r) where
  membership = Here
  {-# INLINE membership #-}

instance Member e r => Member e (_e ': r) where
  membership = There membership
  {-# INLINE membership #-}

instance TypeError (     'Text "Unhandled effect: " ':<>: 'ShowType e
                   ':$$: 'Text "You need to add an interpreter for it."
                   )
                => Member e '[] where
  membership = error "impossible"
