module Control.Monad.Trans.Free.Church.Alternate where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Base
import qualified Control.Monad.Fail as Fail
import Control.Effect.Internal.Union
import Control.Effect.Internal.Utils
import Control.Effect.Type.Unravel
import Control.Effect.Type.ListenPrim
import Control.Effect.Type.ReaderPrim
import Control.Effect.Type.Regional
import Control.Effect.Type.Optional
import Control.Monad.Catch hiding (handle)

newtype FreeT f (m :: * -> *) a = FreeT {
    unFreeT :: forall r
             . (forall x. f x -> (x -> m r) -> m r)
            -> (a -> m r) -> m r
  }

data Coyoneda f a where
  Coyoneda :: (x -> a) -> f x -> Coyoneda f a

newtype LayeredFreeT f m a = LayeredFreeT {
    unLayeredFreeT :: m (Either a (Coyoneda f (LayeredFreeT f m a)))
  }

toLayeredFreeT :: Monad m => FreeT f m a -> LayeredFreeT f m a
toLayeredFreeT free =
  LayeredFreeT $ unFreeT free
                         (\fx c -> return $ Right $ Coyoneda (LayeredFreeT #. c) fx)
                         (return . Left)

fromLayeredFreeT :: Monad m => LayeredFreeT f m a -> FreeT f m a
fromLayeredFreeT stack0 = FreeT $ \h c ->
  let
    go stack = unLayeredFreeT stack >>= \case
      Left a -> c a
      Right (Coyoneda cn f) -> h f (go . cn)
  in
    go stack0

class    (forall f. Threads (FreeT f) p) => FreeThreads p
instance (forall f. Threads (FreeT f) p) => FreeThreads p

liftF :: f a -> FreeT f m a
liftF f = FreeT $ \h c -> f `h` c
{-# INLINE liftF #-}

foldFreeT :: Monad m
          => (a -> b)
          -> (forall x. (x -> m b) -> f x -> m b)
          -> FreeT f m a
          -> m b
foldFreeT b c free = unFreeT free (flip c) (return . b)
{-# INLINE foldFreeT #-}

foldFreeT' :: Monad m
           => (m r -> r)
           -> (a -> r)
           -> (forall x. (x -> r) -> f x -> r)
           -> FreeT f m a
           -> r
foldFreeT' bind b h free =
  bind $ unFreeT free (\fx c -> return $ h (bind . c) fx) (return . b)
{-# INLINE foldFreeT' #-}

instance Functor (FreeT f m) where
  fmap f cnt = FreeT $ \handler c ->
    unFreeT cnt handler (c . f)
  {-# INLINE fmap #-}

instance Applicative (FreeT f m) where
  pure a = FreeT $ \_ c -> c a
  {-# INLINE pure #-}

  ff <*> fa = FreeT $ \handler c ->
    unFreeT ff handler $ \f ->
    unFreeT fa handler (c . f)
  {-# INLINE (<*>) #-}

  liftA2 f fa fb = FreeT $ \handler c ->
    unFreeT fa handler $ \a ->
    unFreeT fb handler (c . f a)
  {-# INLINE liftA2 #-}

  fa *> fb = fa >>= \_ -> fb
  {-# INLINE (*>) #-}

instance Monad (FreeT f m) where
  m >>= f = FreeT $ \handler c ->
    unFreeT m handler $ \a ->
    unFreeT (f a) handler c
  {-# INLINE (>>=) #-}

instance MonadBase b m => MonadBase b (FreeT f m) where
  liftBase = lift . liftBase
  {-# INLINE liftBase #-}

instance Fail.MonadFail m => Fail.MonadFail (FreeT f m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance MonadTrans (FreeT f) where
  lift m = FreeT $ \_ c -> m >>= c
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (FreeT f m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadThrow m => MonadThrow (FreeT f m) where
  throwM = lift . throwM
  {-# INLINE throwM #-}

instance MonadCatch m => MonadCatch (FreeT f m) where
  catch main handle = FreeT $ \handler c ->
    foldFreeT' (join . (`catch` \e -> return $ unFreeT (handle e) handler c))
               c
               (flip handler)
               main
  {-# INLINE catch #-}

instance Monoid w => ThreadsEff (FreeT f) (ListenPrim w) where
  threadEff = threadListenPrim $ \alg main -> FreeT $ \h c ->
    foldFreeT' (\m acc -> alg (ListenPrimListen m) >>= \(s', f) -> f $! acc <> s'
               )
               (\a acc -> c (acc, a))
               (\cn fx acc -> h fx (`cn` acc))
               main
               mempty

instance ThreadsEff (FreeT f) (Regional s) where
  threadEff = threadRegionalViaOptional
  {-# INLINE threadEff #-}

instance Functor s => ThreadsEff (FreeT f) (Optional s) where
  threadEff alg (Optionally sa main) = FreeT $ \h c ->
    foldFreeT' (\m -> join $ alg $ Optionally (fmap c sa) m)
               c
               (flip h)
               main
  {-# INLINE threadEff #-}

instance ThreadsEff (FreeT f) (Unravel p) where
  threadEff alg (Unravel p cataM main) =
    let
      n = \m' -> alg (Unravel p (cataM . lift) m')
    in
      lift $ n
           $ unFreeT main
                     (\fx cn -> return $ cataM $ liftF fx >>= lift . n . cn)
                     return
  {-# INLINE threadEff #-}

instance ThreadsEff (FreeT f) (ReaderPrim i) where
  threadEff = threadReaderPrimViaRegional
  {-# INLINE threadEff #-}
