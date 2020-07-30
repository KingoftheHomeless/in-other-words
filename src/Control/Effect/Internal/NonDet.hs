{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Internal.NonDet where

import Data.Coerce

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Union

import Control.Effect.Type.NonDet
import Control.Effect.Type.Cull
import Control.Effect.Type.Cut
import Control.Effect.Type.Split
import Control.Effect.Type.Regional

import Control.Effect.Internal.Utils


import qualified Control.Monad.Trans.List.Church as L

type NonDetThreads = Threads L.ListT

newtype LogicC m a = LogicC { unLogicC :: L.ListT m a }
  deriving ( Functor, Applicative, Monad
           , MonadFail, MonadIO, MonadBase b
           , MonadThrow, MonadCatch
           )
  deriving MonadTrans


instance ( Carrier m
         , Threads L.ListT (Prims m)
         ) => Carrier (LogicC m) where
  type Derivs (LogicC m) = Split ': Cull ': Cut ': NonDet ': Derivs m
  type Prims  (LogicC m) = Split ': Regional CullOrCall ': Prims m

  algPrims = powerAlg (coerce (algPrims @(CullCutC m))) $ \case
    Split cn (m :: LogicC m a) -> fmap cn (coerce (L.split @m @a) m)
  {-# INLINE algPrims #-}

  reformulate = addPrim $ coerceReform $ reformulate @(CullCutC m)
  {-# INLINE reformulate #-}

data CullOrCall
  = DoCull
  | DoCall

newtype CullCutC m a = CullCutC { unCullCutC :: L.ListT m a }
  deriving ( Functor, Applicative, Monad
           , MonadFail, MonadIO, MonadBase b
           , MonadThrow, MonadCatch
           )
  deriving MonadTrans

instance ( Carrier m
         , Threads L.ListT (Prims m)
         ) => Carrier (CullCutC m) where
  type Derivs (CullCutC m) = Cull ': Cut ': NonDet ': Derivs m
  type Prims  (CullCutC m) = Regional CullOrCall ': Prims m

  algPrims =
    powerAlg (
      coerce (algPrims @(NonDetC m))
    ) $ \case
        Regional DoCull m -> coerceTrans (L.cull @m) m
        Regional DoCall m -> coerceTrans (L.call @m) m
  {-# INLINE algPrims #-}

  reformulate n alg =
    powerAlg (
    powerAlg (
      coerceReform (reformulate @(NonDetC m)) n (weakenAlg alg)
    ) $ \case
      Cutfail -> n (CullCutC L.cutfail)
      Call m  -> (alg . inj) $ Regional DoCall m
    ) $ \case
      Cull m  -> (alg . inj) $ Regional DoCull m
  {-# INLINE reformulate #-}

newtype NonDetC m a = NonDetC { unNonDetC :: L.ListT m a }
  deriving ( Functor, Applicative, Monad
           , MonadFail, MonadIO, MonadBase b
           , MonadThrow, MonadCatch
           )
  deriving MonadTrans

instance ( Carrier m
         , Threads L.ListT (Prims m)
         ) => Carrier (NonDetC m) where
  type Derivs (NonDetC m) = NonDet ': Derivs m
  type Prims  (NonDetC m) = Prims m

  algPrims = coerce (thread @L.ListT (algPrims @m))
  {-# INLINE algPrims #-}

  reformulate n alg =
    powerAlg
      (liftReform reformulate n alg)
      $ \case
        FromList l -> n $ NonDetC $ L.ListT $ \_ c b _ -> foldr c b l
  {-# INLINE reformulate #-}


-- | Runs a 'NonDet' effect.
--
-- Unlike 'runLogic' and 'runCullCut', this doesn't provide any means of interacting
-- with created branches through 'Split', 'Cull' or 'Cut'.
--
-- However, it also doesn't impose any primitive effects, meaning 'runNonDet' doesn't
-- restrict what interpreters are run before it.
--
-- @'Derivs' ('NonDetC' m) = 'NonDet' ': 'Derivs' m@
--
-- @'Prims'  ('NonDetC' m) = 'Prims' m@
runNonDet :: forall f m a p
           . ( Alternative f
             , Carrier m
             , Threaders '[NonDetThreads] m p
             )
          => NonDetC m a
          -> m (f a)
runNonDet = L.runListT .# unNonDetC
{-# INLINE runNonDet #-}

-- | Runs connected 'NonDet', 'Cull', and 'Cut' effects.
--
-- Unlike 'runLogic', this doesn't provide the full power of 'Split'.
-- This allows for a larger variety of interpreters to be run before
-- 'runCullCut' compared to 'runLogic', since 'Split' is significantly harder to
-- thread compared to 'Cull' and 'Cut'.
--
-- @'Derivs' ('CullCutC' m) = 'Cull' ': 'Cut' ': 'NonDet' ': 'Derivs' m@
--
-- @'Prims'  ('CullCutC' m) = 'Regional' CullOrCall ': 'Prims' m@
runCullCut :: forall f m a p
            . ( Alternative f
              , Carrier m
              , Threaders '[NonDetThreads] m p
              )
           => CullCutC m a
           -> m (f a)
runCullCut = L.runListT .# unCullCutC

-- | Runs connected 'NonDet', 'Cull', 'Cut', and 'Split' effects
-- -- i.e. 'Control.Effect.NonDet.Logic'.
--
-- @'Derivs' ('LogicC' m) = 'Split' ': 'Cull' ': 'Cut' ': 'NonDet' ': 'Derivs' m@
--
-- @'Prims'  ('LogicC' m) = 'Split' ': 'Regional' CullOrCall ': 'Prims' m@
--
-- 'Split' is a very restrictive primitive effect. Most notably,
-- interpreters for effects with failure -- such as
-- 'Control.Effect.Error.runError' -- can't be used before 'runLogic'.
-- If you want to use such interpreters before 'runLogic',
-- consider using 'runCullCut' or 'runNonDet' instead.
runLogic :: forall f m a p
          . ( Alternative f
            , Carrier m
            , Threaders '[NonDetThreads] m p
            )
         => LogicC m a
         -> m (f a)
runLogic = L.runListT .# unLogicC
