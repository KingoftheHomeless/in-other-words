module Control.Effect.Error where

import Control.Effect

data Error e m a where
  Throw :: e -> Error e m a
  Catch :: m a -> (e -> m a) -> Error e m a

throw :: Eff (Error e) m => e -> m a
throw = send . Throw

catch :: Eff (Error e) m => m a -> (e -> m a) -> m a
catch m h = send (Catch m h)
