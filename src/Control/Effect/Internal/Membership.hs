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

-- | A constraint that @e@ is part of the effect row @r@.
--
-- @r@ is typically @'Control.Effect.Derivs' m@ for some @m@.
-- @Member e ('Control.Effect.Derivs' m)@ allows you to use
-- actions of @e@ with @m@.
--
-- If @e@ occurs multiple times in @r@, then the first
-- occurence will be used.
--
-- If possible, use @'Control.Effect.Eff'/s@ instead.
class Member e r where
  membership :: ElemOf e r

instance {-# OVERLAPPING #-} Member e (e ': r) where
  membership = Here
  {-# INLINE membership #-}

instance Member e r => Member e (_e ': r) where
  membership = There membership
  {-# INLINE membership #-}

instance TypeError (     'Text "Unhandled effect: " ':<>: 'ShowType e
                   ':$$: 'Text "You need to either add or replace an \
                               \interpreter in your interpretation stack \
                               \so that the effect gets handled."
                   ':$$: 'Text "To check what effects are currently \
                               \handled by your interpretation stack, use \
                               \`debugEffects' from `Control.Effect.Debug'."
                   )
                => Member e '[] where
  membership = error "impossible"
