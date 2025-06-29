{-# LANGUAGE OverloadedStrings #-}

module Physics.ForceSpec (spec) where

import Test.Hspec
import Physics.Force
import Components           (Component(AtomicC))
import Physics.LeapfrogNR   (Vec3)
import NState               (NState)

spec :: Spec
spec = describe "Physics.Force" $ do
  let c     = AtomicC "x"
      dummy :: NState
      dummy = undefined  -- only used by Custom, so never actually inspected

      -- two simple custom forces for (<+>)
      f1, f2 :: Force
      f1 = Custom $ \_ _ -> (1,2,3)
      f2 = Custom $ \_ _ -> (4,5,6)
      sum12 = (5,7,9)

  it "scaleF scales Gravity" $ do
    scaleF 3 (Gravity 2) `shouldBe` Gravity 6

  it "scaleF scales Spring constant only" $ do
    let i = AtomicC "i"
        j = AtomicC "j"
        k = 5
        ℓ = 2
    scaleF 0.5 (Spring i j k ℓ)
      `shouldBe` Spring i j (0.5 * k) ℓ

  it "scaleF scales Drag coefficient" $ do
    scaleF 4 (Drag 0.5) `shouldBe` Drag 2.0

  it "scaleF wraps Custom in scaled force-field" $ do
    let base = Custom $ \_ _ -> (2,3,4)
        sf   = scaleF 2 base
    case sf of
      Custom ff -> ff dummy c `shouldBe` (4,6,8)
      _         -> expectationFailure "Expected Custom after scaleF"

  it "(<+>) on two Customs sums their vectors" $ do
    let comb = f1 <+> f2
    case comb of
      Custom ff -> ff dummy c `shouldBe` sum12
      _         -> expectationFailure "Expected Custom from (<+>)"

  it "(<+>) is commutative for Custom" $ do
    let a = f1 <+> f2
        b = f2 <+> f1
    case (a,b) of
      (Custom ff, Custom gg) ->
        ff dummy c `shouldBe` gg dummy c
      _ -> expectationFailure "Expected both to be Custom"
