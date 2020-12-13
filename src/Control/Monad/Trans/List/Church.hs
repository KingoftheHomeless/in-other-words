module Control.Monad.Trans.List.Church where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import qualified Control.Monad.Catch as C
import qualified Control.Monad.Fail as Fail

import Control.Effect.Carrier

import Control.Effect.Type.ListenPrim
import Control.Effect.Type.WriterPrim
import Control.Effect.Type.Regional
import Control.Effect.Type.Optional
import Control.Effect.Type.Unravel
import Control.Effect.Type.ReaderPrim

newtype ListT (m :: * -> *) a = ListT {
  unListT :: forall r
           . (a -> m r -> m r)
          -> m r -- lose
          -> m r -- cutfail
          -> m r
  }


cons :: a -> ListT m a -> ListT m a
cons a m = ListT $ \c b t -> c a (unListT m c b t)

instance ThreadsEff ListT (Regional s) where
  threadEff = threadRegionalViaOptional

instance Functor s => ThreadsEff ListT (Optional s) where
  threadEff alg (Optionally s main) = ListT $ \c b t ->
    let
      n = \m -> alg (Optionally (fmap (`c` b) s) m)
    in
      join $ n
           $ unListT main
                     (\a r -> return $ c a (join (n r)))
                     (return b)
                     (return t)
  {-# INLINE threadEff #-}

instance ThreadsEff ListT (Unravel p) where
  threadEff alg (Unravel p cataM main) =
    let
      n = \m' -> alg (Unravel p (cataM . lift) m')
    in
      lift $ n
           $ unListT main
                     (\a r -> return $ cataM $ cons a (lift (n r)))
                     (return $ cataM lose)
                     (return $ cataM cutfail)
  {-# INLINE threadEff #-}

instance Monoid s => ThreadsEff ListT (ListenPrim s) where
  threadEff = threadListenPrim $ \alg main ->
    let
      listenStack m = LayeredListT $ do
        (s, e) <- alg (ListenPrimListen (unLayeredListT m))
        case e of
          Right (a, r) -> return $ Right ((s, a), listenStack r)
          Left failure -> return $ Left failure
    in
      (fromLayeredListT . listenStack . toLayeredListT) $ main
  {-# INLINE threadEff #-}

instance Monoid s => ThreadsEff ListT (WriterPrim s) where
  threadEff = threadWriterPrim $ \alg main ->
    let
      passStack m = LayeredListT $ alg $ WriterPrimPass $
        unLayeredListT m >>= \case
          Right ((f, a), r) -> return $ (f, Right (a, passStack r))
          Left e -> return $ (id, Left e)
    in
      (fromLayeredListT . passStack . toLayeredListT) $ main
  {-# INLINE threadEff #-}

instance ThreadsEff ListT (ReaderPrim i) where
  threadEff = threadReaderPrimViaRegional
  {-# INLINE threadEff #-}

instance MonadBase b m => MonadBase b (ListT m) where
  liftBase = lift . liftBase
  {-# INLINE liftBase #-}

instance Fail.MonadFail m => Fail.MonadFail (ListT m) where
  fail = lift . Fail.fail
  {-# INLINE fail #-}

instance MonadThrow m => MonadThrow (ListT m) where
  throwM = lift . C.throwM
  {-# INLINE throwM #-}

instance MonadCatch m => MonadCatch (ListT m) where
  catch main h = ListT $ \c b t ->
    let
      n = (`C.catch` \e -> return $ unListT (h e) c b t)
    in
      join $ n
           $ unListT main
                     (\a r -> return $ c a (join (n r)))
                     (return b)
                     (return t)
  {-# INLINE catch #-}

cull :: ListT m a -> ListT m a
cull m = ListT $ \c b t -> unListT m (\a _ -> c a b) b t

choose :: ListT m a -> ListT m a -> ListT m a
choose ma mb = ListT $ \c b t -> unListT ma c (unListT mb c b t) t

lose :: ListT m a
lose = ListT $ \_ b _ -> b

cutfail :: ListT m a
cutfail = ListT $ \_ _ t -> t

call :: ListT m a -> ListT m a
call m = ListT $ \c b _ -> unListT m c b b

data LoseOrCutfail
  = Lost
  | Cutfailed

newtype LayeredListT m a =
  LayeredListT {
    unLayeredListT :: m (Either LoseOrCutfail (a, LayeredListT m a))
  }

toLayeredListT :: Monad m => ListT m a -> LayeredListT m a
toLayeredListT main =
    LayeredListT
  $ unListT main
            (\a mr -> return $ Right $ (a, LayeredListT mr))
            (return $ Left Lost)
            (return $ Left Cutfailed)

split' :: Monad m => LayeredListT m a -> LayeredListT m (Maybe (a, LayeredListT m a))
split' m = LayeredListT $ unLayeredListT m >>= \case
  Right (a, r)   -> return $ Right (Just (a, r), LayeredListT $ return $ Left Lost)
  Left Lost      -> return $ Right (Nothing, LayeredListT $ return $ Left Lost)
  Left Cutfailed -> return $ Left Cutfailed

fromLayeredListT :: Monad m => LayeredListT m a -> ListT m a
fromLayeredListT main = ListT $ \c b t ->
  let
    go m = unLayeredListT m >>= \case
      Left Lost -> b
      Left Cutfailed -> t
      Right (a, r) -> c a (go r)
  in
    go main

-- split cutfail === cutfail
-- If you don't want that behaviour, instead of @split m@, do @split (call m)@
split :: Monad m => ListT m a -> ListT m (Maybe (a, ListT m a))
split =
   (fmap . fmap . fmap) fromLayeredListT
  . fromLayeredListT
  . split'
  . toLayeredListT
{-# INLINE split #-}

instance Functor (ListT m) where
  fmap f m = ListT $ \c b t ->
    unListT m (c . f) b t
  {-# INLINE fmap #-}

instance Applicative (ListT m) where
  pure a = ListT $ \c b _ -> c a b
  liftA2 f fa fb = ListT $ \c b t ->
    unListT fa (\a r -> unListT fb (c . f a) r t) b t
  {-# INLINE liftA2 #-}

  ma *> mb = ma >>= \_ -> mb
  {-# INLINE (*>) #-}

instance Monad (ListT m) where
  m >>= f = ListT $ \c b t ->
    unListT m (\a r -> unListT (f a) c r t) b t
  {-# INLINE (>>=) #-}

instance MonadTrans ListT where
  lift m = ListT $ \c b _ -> m >>= (`c` b)
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (ListT m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

runListT :: (Alternative f, Monad m)
         => ListT m a
         -> m (f a)
runListT m =
  unListT m (fmap . (<|>) . pure) (pure empty) (pure empty)
{-# INLINE runListT #-}


