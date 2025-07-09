{-# LANGUAGE OverloadedStrings #-}

module Physics.Collision.BoundingVolumeSpec (spec) where

import Test.Hspec
import Physics.Collision.BoundingVolume
import Physics.Collision.Types (AABB(..), SphereBB(..))
import Physics.Math.Vec3Util   (Vec3)

spec :: Spec
spec = describe "Physics.Collision.BoundingVolume" $ do
  let a1 = AABB (0,0,0) (1,1,1)
      a2 = AABB (0.5,0.5,0.5) (2,2,2)
      a3 = AABB (2,2,2) (3,3,3)
      s1 = SphereBB (0,0,0) 1
      s2 = SphereBB (1.5,0,0) 1
      s3 = SphereBB (3,0,0) 1

  it "intersectsAABB detects overlap" $
    intersectsAABB a1 a2 `shouldBe` True

  it "intersectsAABB detects no overlap" $
    intersectsAABB a1 a3 `shouldBe` False

  it "intersectsSphere detects overlap" $
    intersectsSphere s1 s2 `shouldBe` True

  it "intersectsSphere detects no overlap" $
    intersectsSphere s1 s3 `shouldBe` False

  it "aabbUnion encloses both inputs" $ do
    let u = aabbUnion a1 a3
    aMin u `shouldBe` (0,0,0)
    aMax u `shouldBe` (3,3,3)

  it "aabbFromSphere matches center ± radius" $ do
    let sb = SphereBB (1,2,3) 1.5
        AABB mn mx = aabbFromSphere sb
    mn `shouldBe` (-0.5,0.5,1.5)
    mx `shouldBe` (2.5,3.5,4.5)
