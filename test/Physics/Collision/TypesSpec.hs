{-# LANGUAGE OverloadedStrings #-}

module Physics.Collision.TypesSpec (spec) where

import Test.Hspec
import Physics.Collision.Types
import Components (Component(AtomicC))

spec :: Spec
spec = describe "Physics.Collision.Types" $ do
  let c      = AtomicC "test"
      aabb   = AABB (0,0,0) (1,1,1)
      sphere = SphereBB (2,2,2) 1.5
      obb    = OBB (3,3,3) (1,2,3) (1,0,0,0)
  it "AABB equality and show" $ do
    aabb `shouldBe` aabb
    show aabb `shouldContain` "AABB"
  it "SphereBB equality and show" $ do
    sphere `shouldBe` sphere
    show sphere `shouldContain` "SphereBB"
  it "OBB equality and show" $ do
    obb `shouldBe` obb
    show obb `shouldContain` "OBB"
  it "ComponentBB pairs component and BV" $ do
    let cb = (c, BB_AABB aabb)
    fst cb `shouldBe` c
    case snd cb of
      BB_AABB x -> x `shouldBe` aabb
      _         -> expectationFailure "Expected AABB"
