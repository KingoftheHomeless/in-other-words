module Control.Effect.Tagged
  ( -- * Effect
    Tagged(..)
  , Labeled
  , LabeledBy

    -- * Actions
  , tag

  , label

    -- * Interpretations
  , untag
  , retag

  , unlabel
  , relabel
  ) where

import Control.Effect

import Control.Effect.Internal.Newtype

-- | An effect for annotating effects and disambiguating identical effects.
newtype Tagged l (e :: Effect) m a = Tagged (e m a)

type TagC l e = WrapC e (Tagged l e)

-- | Tag uses of an effect, effectively gaining access to the tagged effect
-- locally.
--
-- This may be used to create tagged- variants of regular actions.
--
-- For example:
--
-- @
-- taggedLocal :: forall l i r a
--              . 'Eff' ('Tagged' l ('Control.Effect.Reader.Reader' i)) m
--             => (i -> i)
--             -> m a
--             -> m a
-- taggedLocal f m =
--   'tag' @l @('Control.Effect.Reader.Reader' i) $ 'Control.Effect.Reader.local' @i f ('lift' m)
-- @
tag :: Eff (Tagged l e) m => TagC l e m a -> m a
tag = wrap
{-# INLINE tag #-}

type UntagC l e = UnwrapC e (Tagged l e)

-- | Rewrite a @'Tagged' l e@ effect into @e@.
untag :: forall l e m a
       . HeadEff e m
      => UntagC l e m a
      -> m a
untag = unwrap
{-# INLINE untag #-}

type RetagC s s' e = WrapC (Tagged s e) (Tagged s' e)

-- | Transform a @'Tagged' l e@ to a @'Tagged' l' e@ effect.
retag :: forall l l' e m a
       . Eff (Tagged l' e) m
      => RetagC l l' e m a
      -> m a
retag = wrap
{-# INLINE retag #-}

-- | An open type family for labels with an associated effect.
--
-- You can be used together with 'Labeled' as a variant of 'Tagged' that is
-- unambiguous in what effect is being tagged.
--
-- For example:
--
-- @
-- data AppleStock (e :: k)
--
-- type instance 'LabeledBy' AppleStock = 'Control.Effect.State.State' Int
--
-- getApplesInStock :: 'Eff' ('Labeled' AppleStock) m => m Int
-- getApplesInStock = label @AppleStock (get @Int)
-- @
--
type family LabeledBy (l :: k) :: Effect

-- | A variant of 'Tagged' where the effect is given by the label.
type Labeled l = Tagged l (LabeledBy l)

type LabelC l = TagC l (LabeledBy l)

-- | Label uses of an effect, effectively gaining acces to that effect
-- within a region.
label :: Eff (Labeled l) m => LabelC l m a -> m a
label = tag
{-# INLINE label #-}

type UnlabelC l = UntagC l (LabeledBy l)

-- | Rewrite a @'Labeled' l@ effect into the effect that is @'LabeledBy' l@.
unlabel :: HeadEff (LabeledBy l) m
        => UnlabelC l m a
        -> m a
unlabel = untag
{-# INLINE unlabel #-}

type RelabelC l l' = RetagC l l' (LabeledBy l)

-- | Transform a @'Labeled' l@ effect into another @'Labeled' l'@, as long
-- as @l@ and @l'@ both label the same effect.
relabel :: forall l l' m a
         . ( Eff (Labeled l') m
           , LabeledBy l ~ LabeledBy l'
           )
        => RelabelC l l' m a
        -> m a
relabel = retag
{-# INLINE relabel #-}
