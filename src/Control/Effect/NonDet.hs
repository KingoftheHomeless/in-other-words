module Control.Effect.NonDet
  ( -- * Effects
    NonDet(..)
  , Cull(..)
  , Cut(..)
  , Split(..)
  , Logic

    -- * Actions
  , choose
  , lose
  , fromList

  , cull

  , cutfail
  , cut
  , call

  , split

    -- * Interpretations
  , runNonDet
  , runNonDet1

  , runCullCut

  , runLogic

    -- * Threading constraints
  , NonDetThreads

    -- * Carriers
  , NonDetC
  , CullCutC
  , LogicC
  ) where

import Control.Monad

import Control.Effect
import Control.Effect.Internal.NonDet
import Control.Effect.Internal.Utils

import Control.Effect.Type.Split

-- | Introduce new branches stemming from the current one using a list of values.
fromList :: Eff NonDet m => [a] -> m a
fromList = send .# FromList

-- | Introduce two new branches stemming from the current one.
choose :: Eff NonDet m => m a -> m a -> m a
choose ma mb = join $ fromList [ma, mb]
{-# INLINE choose #-}

-- | Fail the current branch and proceed to the next branch,
-- backtracking to the nearest use of 'choose' or 'fromList' that
-- still has unprocessed branches.
lose :: Eff NonDet m => m a
lose = fromList []
{-# INLINE lose #-}

-- | Cull nondeterminism in the argument, limiting the number of branches
-- it may introduce to be at most 1.
--
-- @'cull' (return True \`'choose'\` return False) == return True@
--
-- @'cull' ('lose' \`'choose'\` return False) == return False@
cull :: Eff Cull m => m a -> m a
cull = send .# Cull
{-# INLINE cull #-}

-- | Fail the current branch, and prevent backtracking up until the nearest
-- enclosing use of 'call' (if any).
--
-- @'cutfail' \`'choose'\` m == 'cutfail'@
cutfail :: Eff Cut m => m a
cutfail = send Cutfail
{-# INLINE cutfail #-}

-- | Commit to the current branch: prevent all backtracking that would move
-- execution to before 'cut' was invoked, up until the nearest enclosing use
-- of 'call' (if any).
--
-- @'call' ('fromList' [1,2] >>= \\a -> 'cut' >> 'fromList' [a,a+3]) == 'fromList' [1,4]@
--
-- @'call' (('cut' >> return True) \`'choose'\` return False) == return True@
cut :: Effs '[NonDet, Cut] m => m ()
cut = pure () `choose` cutfail
{-# INLINE cut #-}

-- | Delimit the prevention of backtracking from uses of 'cut' and 'cutfail'.
--
-- @'call' 'cutfail' \`'choose'\` m = m@
call :: Eff Cut m => m a -> m a
call = send . Call
{-# INLINE call #-}

-- | Split a nondeterministic computation into its first result
-- and the rest of the computation, if possible.
--
-- Note that @'split' 'cutfail' == 'cutfail'@. If you don't want that behavior,
-- use @'split' ('call' m)@ instead of @'split' m@.
split :: Eff Split m => m a -> m (Maybe (a, m a))
split = send . Split id
{-# INLINE split #-}
