{-# LANGUAGE OverloadedStrings #-}

module Physics.Collision.ManagerSpec (spec) where

import Test.Hspec
import Physics.Collision.Manager
import Physics.Collision.Types        (AABB(..), BoundingVolume(..))
import Physics.Collision.BroadPhase.SweepAndPrune as SAP
import Physics.Collision.BroadPhase.UniformGrid    as UG
import Components                              (Component(AtomicC))
import qualified Data.Set                     as S
import Physics.Math.Vec3Util                  (Vec3)

spec :: Spec
spec = describe "Physics.Collision.Manager" $ do
  let c1    = AtomicC "a"
      c2    = AtomicC "b"
      a1    = AABB (0,0,0)     (1,1,1)
      a2    = AABB (0.5,0.5,0.5) (1.5,1.5,1.5)
      bb1   = (c1, BB_AABB a1)
      bb2   = (c2, BB_AABB a2)
      bbs   = [bb1, bb2]
      expected = S.singleton (c1,c2)

  it "buildManager BP_SAP produces CM_SAP" $ do
    let cm = buildManager BP_SAP bbs
    runBroadPhase cm `shouldBe` expected

  it "buildManager BP_Grid produces CM_Grid" $ do
    let cm = buildManager (BP_Grid 1.0) bbs
    runBroadPhase cm `shouldBe` expected

  it "updateManager SAP updates entries" $ do
    let cm0 = buildManager BP_SAP []
        cm1 = updateManager cm0 bbs
    runBroadPhase cm1 `shouldBe` expected

  it "updateManager Grid updates entries" $ do
    let cm0 = buildManager (BP_Grid 1.0) []
        cm1 = updateManager cm0 bbs
    runBroadPhase cm1 `shouldBe` expected
