{-# LANGUAGE OverloadedStrings #-}
module ScalarLitSpec (spec) where

import Test.Hspec
import Components      (Component(..))
import ScalarLiteral

spec :: Spec
spec = describe "ScalarLiteral" $ do
  let c = AtomicC "x"

  it "lookupSL defaults to 0" $
       lookupSL c emptySL `shouldBe` 0

  it "insertSL / lookupSL round-trip" $
       lookupSL c (insertSL c 3 emptySL) `shouldBe` 3

  it "adjustSL updates or inserts" $
       let sl  = adjustSL (+1) 0 c emptySL   -- inserts 0 then +1
           sl' = adjustSL (+1) 0 c sl        -- now 1→2
       in lookupSL c sl' `shouldBe` 2
