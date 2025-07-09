{-# LANGUAGE OverloadedStrings #-}

module Physics.Collision.BroadPhase.SAPSpec (spec) where

import Test.Hspec
import Physics.Collision.BroadPhase.SweepAndPrune
import Physics.Collision.Types       (AABB(..), BoundingVolume(..))
import Physics.Math.Vec3Util        (Vec3)
import Components                    (Component(AtomicC))
import qualified Data.Set as S

spec :: Spec
spec = describe "Physics.Collision.BroadPhase.SweepAndPrune" $ do
  let c1   = AtomicC "a"
      c2   = AtomicC "b"
      c3   = AtomicC "c"
      a1   = AABB (0,0,0) (1,1,1)
      a2   = AABB (0.5,0.5,0.5) (1.5,1.5,1.5)
      a3   = AABB (2,2,2) (3,3,3)
      bb1  = (c1, BB_AABB a1)
      bb2  = (c2, BB_AABB a2)
      bb3  = (c3, BB_AABB a3)

  it "initSAP with empty yields no pairs" $
    potentialPairsSAP (initSAP []) `shouldBe` S.empty

  it "two overlapping boxes yields one pair" $
    potentialPairsSAP (initSAP [bb1,bb2])
      `shouldBe` S.singleton (c1,c2)

  it "two non-overlapping boxes yields no pairs" $
    potentialPairsSAP (initSAP [bb1,bb3]) `shouldBe` S.empty

  it "three boxes yields correct subset of pairs" $
    let sap = initSAP [bb1,bb2,bb3]
    in potentialPairsSAP sap `shouldBe` S.singleton (c1,c2)

  it "updateSAP replaces entries" $ do
    let sap1 = initSAP [bb1]
        sap2 = updateSAP sap1 [bb2]
    potentialPairsSAP sap2 `shouldBe` S.empty
