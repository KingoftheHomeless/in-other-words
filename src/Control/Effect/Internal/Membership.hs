module Control.Effect.Internal.Membership where

import Data.Kind (Constraint)

data ElemOf (e :: a) (r :: [a]) where
  Here  :: ElemOf e (e ': r)
  There :: ElemOf e r -> ElemOf e (e' ': r)


deriving instance Show (ElemOf e r)

class Member e r where
  membership :: ElemOf e r

instance {-# OVERLAPPING #-} Member e (e ': r) where
  membership = Here
  {-# INLINE membership #-}

instance Member e r => Member e (_e ': r) where
  membership = There $ membership @e @r
  {-# INLINE membership #-}


type family Members (xs :: [a]) (r :: [a]) :: Constraint where
  Members '[] r = ()
  Members (x ': xs) r = (Member x r, Members xs r)
