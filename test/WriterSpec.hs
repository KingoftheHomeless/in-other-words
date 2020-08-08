{-# LANGUAGE AllowAmbiguousTypes #-}
module WriterSpec where

import Test.Hspec

import qualified Control.Concurrent.Async as A
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (evaluate)

import Control.Effect
import Control.Effect.Conc
import Control.Effect.Error
import Control.Effect.Reader
import Control.Effect.Writer
import Control.Effect.Unlift

censor' :: forall e s a m
        . Effs '[Error e, Writer s] m
        => (s -> s)
        -> m a
        -> m a
censor' f m = do
  res <- censor f $ try m
  case res of
      Right res' -> return res'
      Left e -> throw (e :: e)

test1 :: (String, Either () ())
test1 =
    run
  $ runWriter
  $ runError $ do
    tell "censoring"
    censor @String
      (drop 4)
      (tell " not applied" *> throw ())
    `catch`
      (\(_ :: ()) -> pure ())

test2 :: (String, Either () ())
test2 =
    run
  $ runWriter
  $ runError $
  do
    tell "censoring"
    censor' @() @String
      (drop 4)
      (tell " not applied" *> throw ())
    `catch`
      (\(_ :: ()) -> pure ())

test3 :: (String, (String, ()))
test3 = run . runListen $ listen (tell "and hear")

test4 :: IO (String, String)
test4 = do
  tvar <- newTVarIO ""
  (listened, _) <- runM $ concToIO $ runWriterTVar tvar $ do
    tell "message "
    listen $ do
      tell "has been"
      a <- async $ tell " received"
      wait a
  end <- readTVarIO tvar
  return (end, listened)

test5 :: IO (String, String)
test5 = do
  tvar <- newTVarIO ""
  lock <- newEmptyMVar
  (listened, a) <- runM $ concToIO $ runWriterTVar tvar $ do
    tell "message "
    listen $ do
      tell "has been"
      a <- async $ do
        embed $ takeMVar lock
        tell " received"
      return a
  putMVar lock ()
  _ <- A.wait a
  end <- readTVarIO tvar
  return (end, listened)

test6 :: ( Effs '[Error (Async ()), Embed IO] m
         , Threaders '[ReaderThreads] m p
         , MonadMask m
         , MonadBaseControlPure IO m
         )
      => m String
test6 = do
  tvar <- embed $ newTVarIO ""
  lock <- embed $ newEmptyMVar
  let
      inner = do
        tell "message "
        fmap snd $ listen @String $ do
          tell "has been"
          a <- async $ do
            embed $ takeMVar lock
            tell " received"
          throw a
  concToIO (runWriterTVar tvar inner) `catch` \a ->
    embed $ do
      putMVar lock ()
      () <- A.wait a
      readTVarIO tvar



spec :: Spec
spec = do
  describe "writer" $ do
    it "should not censor" $ do
      test1 `shouldBe` ("censoring not applied", Right ())

    it "should censor" $ do
      test2 `shouldBe` ("censoring applied", Right ())

    it "should have a proper listen" $ do
      test3 `shouldBe` ("and hear", ("and hear", ()))

    it "should be strict in the output" $
      let
        t1 :: (Carrier m, Threaders '[WriterThreads] m p) => m (String, ())
        t1 = runWriter @String $ do
          tell @String (error "strict")
          return ()

        t2 :: (Carrier m, Threaders '[WriterThreads] m p) => m (String, ())
        t2 = runWriter @String $ do
          _ <- listen @String (tell @String (error "strict"))
          return ()

        t3 :: (Carrier m, Threaders '[WriterThreads] m p) => m (String, ())
        t3 = runWriter @String $ do
          pass @String $ pure (\_ -> error "strict", ())
          return ()
      in do
        runM t1           `shouldThrow` errorCall "strict"
        evaluate (run t1) `shouldThrow` errorCall "strict"
        runM t2           `shouldThrow` errorCall "strict"
        evaluate (run t2) `shouldThrow` errorCall "strict"
        runM t3           `shouldThrow` errorCall "strict"
        evaluate (run t3) `shouldThrow` errorCall "strict"

  describe "runWriterTVar" $ do
    it "should listen and commit asyncs spawned and awaited upon in a listen \
       \block" $ do
      (end, listened) <- test4
      end `shouldBe` "message has been received"
      listened `shouldBe` "has been received"

    it "should commit writes of asyncs spawned inside a listen block even if \
       \the block has finished." $ do
      (end, listened) <- test5
      end `shouldBe` "message has been received"
      listened `shouldBe` "has been"


    it "should commit writes of asyncs spawned inside a listen block even if \
       \the block failed for any reason." $ do
      Right end1 <- runM $ errorToIO @(Async ()) $ test6
      end1 `shouldBe` "message has been received"

  describe "runLazyWriter" $ do
    let
      runLazily     = run . runAskConstSimple () . runWriterLazy @[Int]
      runSemiLazily = runLazily . runError @()
      runStrictly   = run . runError @() . runWriterLazy @[Int]
      runStrictlyM  = runM . runWriterLazy @[Int]

      act :: Eff (Writer [Int]) m => m ()
      act = do
        tell @[Int] [1]
        tell @[Int] [2]
        error "strict"

    it "should build the final output lazily, if the interpreters after \
       \runLazyWriter and the final monad are lazy" $ do
      (take 2 . fst . runLazily) act `shouldBe` [1,2]
      (take 2 . fst . runSemiLazily) act `shouldBe` [1,2]
      evaluate (runStrictly act) `shouldThrow` errorCall "strict"
      runStrictlyM act `shouldThrow` errorCall "strict"

    it "should listen lazily if all interpreters and final monad are lazy" $ do
      let
        listenAct :: Eff (Writer [Int]) m => m [Int]
        listenAct = do
          (end,_) <- listen @[Int] act
          return (take 2 end)
      (snd . runLazily) listenAct `shouldBe` [1,2]

      evaluate ((snd . runSemiLazily) listenAct) `shouldThrow` errorCall "strict"
      evaluate (runStrictly listenAct) `shouldThrow` errorCall "strict"
      runStrictlyM listenAct `shouldThrow` errorCall "strict"

    it "should censor lazily if all interpreters and final monad are lazy" $ do
      let
        censorAct :: Eff (Writer [Int]) m => m ()
        censorAct = censor @[Int] (\(_:y:_) -> [0,y]) act
      (fst . runLazily) censorAct `shouldBe` [0,2]
      evaluate ((fst . runSemiLazily) censorAct) `shouldThrow` errorCall "strict"
      evaluate (runStrictly censorAct) `shouldThrow` errorCall "strict"
      runStrictlyM censorAct `shouldThrow` errorCall "strict"
