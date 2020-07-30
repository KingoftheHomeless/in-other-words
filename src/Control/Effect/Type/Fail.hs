{-# OPTIONS_HADDOCK not-home #-}
module Control.Effect.Type.Fail where

-- | An effect corresponding to the 'Control.Monad.Fail.MonadFail' type class.
--
-- 'Control.Effect.Effly.Effly''s 'Control.Monad.Fail.MonadFail' instance is based
-- on this effect; by having access to 'Fail', you're able to invoke
-- handle pattern-match failure automatically inside of effect handlers.
newtype Fail m a where
  Fail :: String -> Fail m a