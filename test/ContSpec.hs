module ContSpec where

import Control.Effect
import Control.Effect.Cont
import Control.Effect.Reader

import Test.Hspec

test :: Effs '[Shift (Int, Int), Reader Int] m => m (Int, Int)
test = do
  i <- local @Int (*2) $
       shift @(Int, Int) $ \c ->
             local @Int (*3) (ask @Int)
         >>= local @Int (*5) . c
  j <- local @Int (*7) $ ask @Int
  return (i, j)

test1 :: Effs '[Shift (Int, Int), Reader Int] m => m (Int, Int)
test1 = do
  i <- shift @(Int, Int) $ \c ->
             local @Int (*3) (ask @Int)
         >>= local @Int (*5) . c
  j <- local @Int (*7) $ ask @Int
  return (i, j)

spec :: Spec
spec = do
  describe "runShift" $ do
    it "neither local nor global HO-actions should affect a continuation,\
       \ whether applied inside or outside a 'shift'" $ do
      let (i, j) = run $ runReader @Int 1 $ runShift $ test
      i `shouldBe` 6 -- 2 * 3
      j `shouldBe` 7
      let (i', j') = run $ runShift $ runReader @Int 1 $ test
      i' `shouldBe` 6
      j' `shouldBe` 7


  describe "runShiftFast" $ do
    it "should have horrible semantics when threading ReaderPrim. \
       \See Issue #6." $ do
      let (i, j) = run $ runReader @Int 1 $ runShiftFast $ test
      i `shouldBe` 6
      j `shouldNotBe` 35 -- This is what we actually want it to be
      j `shouldBe` 7
      let (i', j') = run $ runReader @Int 1 $ runShiftFast $ test1
      i' `shouldBe` 3
      j' `shouldBe` 35
      let (i'', j'') = run $ runShiftFast $ runReader @Int 1 $ test
      i'' `shouldBe` 6
      j'' `shouldBe` 7
