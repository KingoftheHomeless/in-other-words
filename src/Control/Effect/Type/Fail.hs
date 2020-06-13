module Control.Effect.Type.Fail where

newtype Fail m a where
  Fail :: String -> Fail m a
