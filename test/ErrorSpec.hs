module ErrorSpec where

import Test.Hspec

import Control.Effect
import Control.Effect.Error
import Control.Effect.Conc


test1 :: IO (Either () (Either () ()))
test1 = runM $ errorToIO $ errorToIO $ do
  (intro1 . intro1) $ throw () `catch` \() -> return ()

test2 :: IO (Either () (Either () ()))
test2 = runM $ errorToIO $ errorToIO $ do
  (intro1 . intro1) (throw ()) `catch` \() -> return ()

test3 :: IO (Either () ())
test3 = runM $ errorToIO $ concToIO $ do
  a <- async $ throw ()
  wait a

test4 :: IO (Either () ())
test4 = runM $ errorToIO $ concToIO $ do
  a <- async $ throw ()
  wait a `catch` \() -> return ()

spec :: Spec
spec = do
  describe "errorToIO" $ do
    it "should catch/throw exceptions only belonging to \
       \the specific interpreted Error" $ do
      res1 <- test1
      res1 `shouldBe` Right (Right ())

      res2 <- test2
      res2 `shouldBe` Left ()
    it "should propagate exceptions thrown in 'async'ed exceptions" $ do
      res3 <- test3
      res3 `shouldBe` Left ()

      res4 <- test4
      res4 `shouldBe` Right ()
