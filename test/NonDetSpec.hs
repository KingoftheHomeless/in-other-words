module NonDetSpec where

import Test.Hspec

import Control.Effect
import Control.Effect.Alt
import Control.Effect.NonDet
import Control.Effect.Trace
import Control.Effect.Reader

runAlt :: Alternative f => AltToNonDetC (NonDetC RunC) a -> f a
runAlt = run . runNonDet . altToNonDet

inHigherOrder :: Effs '[NonDet, Trace, Reader ()] m
              => m ()
inHigherOrder = altToNonDet $ do
  local (\_ -> ()) $ trace "1" <|> trace "2"
  trace "3"

spec :: Spec
spec = parallel $ do
  describe "runNonDet" $ do
    it "should choose the first branch" $ do
      runAlt (pure '1' <|> pure '2') `shouldBe` (Just '1')
    it "should failover" $ do
      runAlt (empty <|> pure '2') `shouldBe` (Just '2')
      runAlt (pure '1' <|> empty) `shouldBe` (Just '1')

    it "should NOT have terrible semantics when <|> is used within\
       \ a higher-order action of a later effect!" $ do
      (fst . run . runTraceList . runReader () . runNonDet @[]) inHigherOrder
        `shouldNotBe` ["1","2","3","3"]
      (fst . run . runTraceList . runReader () . runNonDet @[]) inHigherOrder
        `shouldBe` ["1","3","2","3"]
      (fst . run . runTraceList . runNonDet @[] . runReader ()) inHigherOrder
        `shouldBe` ["1","3","2","3"]
