module Control.Monad.Trans.List.Church where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans
import qualified Control.Monad.Catch as C
import qualified Control.Monad.Fail as Fail

import Control.Effect.Carrier

import Control.Effect.Type.Listen
import Control.Effect.Type.Pass
import Control.Effect.Type.Regional
import Control.Effect.Type.Optional
import Control.Effect.Type.ReaderPrim

newtype ListT m a = ListT {
  unListT :: forall r
             . (forall x. m x -> (x -> r) -> r)
            -> (a -> r -> r)
            -> r -- empty
            -> r -- cutfail
            -> r
  }

instance ThreadsEff ListT (Regional s) where
  threadEff alg (Regionally s m) = ListT $ \bind ->
    unListT m (bind . alg . Regionally s)
  {-# INLINE threadEff #-}

instance Functor s => ThreadsEff ListT (Optional s) where
  threadEff alg (Optionally s m) = ListT $ \bind c b ->
    unListT m (\mx cn ->
      (`bind` id) $ alg $
        fmap (`c` b) s
      `Optionally`
        fmap cn mx
      ) c b
  {-# INLINE threadEff #-}

instance Monoid s => ThreadsEff ListT (Listen s) where
  threadEff alg (Listen main) = ListT $ \bind c b t ->
    unListT
      main
      (\mx cn acc -> alg (Listen mx) `bind` \(s, a) ->
          let
            !acc' = acc <> s
          in
            cn a acc'
      )
      (\a r acc -> c (acc, a) (r mempty))
      (const b)
      (const t)
      mempty
  {-# INLINE threadEff #-}

instance ThreadsEff ListT (Pass s) where
  threadEff alg (Pass main) =
    let
      go' m = m >>= \case
        Empty         -> return (id, Empty)
        CutFail       -> return (id, CutFail)
        Cons (f, a) r -> return (f, Cons a (go r))
        Embed mx cn   -> go' (fmap cn mx)

      go Empty = Empty
      go CutFail = CutFail
      go (Cons (_, a) r) = Cons a (go r)
      go (Embed mx cn) = (`Embed` id) $ alg $ Pass $ go' (fmap cn mx)
    in
      fromLayeredListT (go (toLayeredListT main))
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
  catch m h = ListT $ \bind c b t ->
    unListT
      m
      (\mx cn -> (`bind` id) $
        fmap cn mx `C.catch` \e -> return $ unListT (h e) bind c b t
      )
      c b t
  {-# INLINE catch #-}

cull :: ListT m a -> ListT m a
cull m = ListT $ \bind c b t -> unListT m bind (\a _ -> c a b) b t

cutfail :: ListT m a
cutfail = ListT $ \_ _ _ t -> t

call :: ListT m a -> ListT m a
call m = ListT $ \bind c b _ -> unListT m bind c b b

data LayeredListT m a where
  Embed   :: m x -> (x -> LayeredListT m a) -> LayeredListT m a
  Empty   :: LayeredListT m a
  CutFail :: LayeredListT m a
  Cons    :: a -> LayeredListT m a -> LayeredListT m a

toLayeredListT :: ListT m a -> LayeredListT m a
toLayeredListT m = unListT m Embed Cons Empty CutFail

split' :: LayeredListT m a -> LayeredListT m (Maybe (a, LayeredListT m a))
split' (Embed mx cn) = Embed mx (split' . cn)
split' Empty         = Cons Nothing Empty
split' CutFail       = CutFail
split' (Cons a r)    = Cons (Just (a, r)) Empty

fromLayeredListT :: LayeredListT m a -> ListT m a
fromLayeredListT m = ListT $ \bind c b t ->
  let
    go (Embed mx cn) = mx `bind` (go . cn)
    go Empty = b
    go CutFail = t
    go (Cons a r) = c a (go r)
  in
    go m

-- split cutfail === cutfail
-- If you don't want that behaviour, instead of @split m@, do @split (call m)@
split :: ListT m a -> ListT m (Maybe (a, ListT m a))
split =
   (fmap . fmap . fmap) fromLayeredListT
  . fromLayeredListT
  . split'
  . toLayeredListT
{-# INLINE split #-}

instance Functor (ListT m) where
  fmap f m = ListT $ \bind c b t ->
    unListT m bind (c . f) b t
  {-# INLINE fmap #-}

instance Applicative (ListT m) where
  pure a = ListT $ \_ c b _ -> c a b
  liftA2 f fa fb = ListT $ \bind c b t ->
    unListT fa bind (\a r -> unListT fb bind (c . f a) r t) b t
  {-# INLINE liftA2 #-}

  ma *> mb = ma >>= \_ -> mb
  {-# INLINE (*>) #-}

instance Monad (ListT m) where
  m >>= f = ListT $ \bind c b t ->
    unListT m bind (\a r -> unListT (f a) bind c r t) b t
  {-# INLINE (>>=) #-}

instance MonadTrans ListT where
  lift m = ListT $ \bind c b _ -> m `bind` (`c` b)
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (ListT m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

runListT :: (Alternative f, Monad m)
         => ListT m a
         -> m (f a)
runListT m =
  unListT m (>>=) (fmap . (<|>) . pure) (pure empty) (pure empty)
{-# INLINE runListT #-}


