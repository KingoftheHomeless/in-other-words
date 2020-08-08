module BracketSpec where

import Test.Hspec

import Control.Concurrent.MVar

import Control.Effect
import Control.Effect.Mask
import Control.Effect.Bracket
import Control.Effect.Error
import Control.Effect.Conc
import Control.Effect.Trace


test1 :: IO ([String], Either () ())
test1 = runM $ runTraceListIO $ errorToIO @() $ bracketToIO $ do
  throw () `onError` trace "protected"

test2 :: IO [String]
test2 = runM $ fmap fst $ runTraceListIO $ maskToIO $ concToIO $ bracketToIO $ do
  a <- mask $ \restore -> async $ do
    restore (embed (newEmptyMVar @()) >>= embed . takeMVar) `onError` trace "protected"
  cancel a

test3 :: IO ([String], Either () ())
test3 = runM $ runTraceListIO $ bracketToIO $ runError $ do
  throw () `onError` trace "protected"

test4 :: IO (Either () [String])
test4 = runM $ runError $ fmap fst $ runTraceListIO $ bracketToIO $ do
  (throw () `onError` trace "protected") `catch` \() -> return ()

test5 :: ([String], Either () ())
test5 = run $ runTraceList $ runBracketLocally $ runError $ do
  throw () `onError` trace "protected"

test6 :: IO ([String], Either () ())
test6 = runM $ runTraceList $ runBracketLocally $ errorToIO $ do
  throw () `onError` trace "protected"

test7 :: Either () [String]
test7 = run $ runError $ fmap fst $ runTraceList $ runBracketLocally $ do
  (throw () `onError` trace "protected") `catch` \() -> return ()

spec :: Spec
spec = do
  describe "bracketToIO" $ do
    it "should protect against IO exceptions" $ do
      res <- test1
      res `shouldBe` (["protected"], Left ())
    it "should protect against asynchronous exceptions" $ do
      res <- test2
      res `shouldBe` ["protected"]
    it "should protect against pure exceptions" $ do
      res3 <- test3
      res3 `shouldBe` (["protected"], Left ())
      res4 <- test4
      res4 `shouldBe` Right ["protected"]

  describe "runBracketLocally" $ do
    it "should protect against pure exceptions of local effects" $ do
      test5 `shouldBe` (["protected"], Left ())
    it "should not protect against global effects" $ do
      res6 <- test6
      res6 `shouldBe` ([], Left ())
      test7 `shouldBe` Right []
