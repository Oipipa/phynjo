{-# LANGUAGE OverloadedStrings #-}

module Physics.RigidStateSpec (spec) where

import Test.Hspec
import Components            (Component(AtomicC))
import Physics.RigidState
import Physics.RigidBody     (Quaternion)
import Physics.LeapfrogNR    (Vec3)

spec :: Spec
spec = describe "Physics.RigidState" $ do

  let c     = AtomicC "body"
      pos   = (1.0,2.0,3.0)             :: Vec3
      ori   = (0.707,0.0,0.707,0.0)     :: Quaternion
      vel   = (4.0,5.0,6.0)             :: Vec3
      ang   = (0.1,0.2,0.3)             :: Vec3

  it "emptyRigid provides default zeros" $ do
    let s0 = emptyRigid
    lookupPosR    c s0 `shouldBe` (0,0,0)
    lookupOriR    c s0 `shouldBe` (1,0,0,0)
    lookupVelR    c s0 `shouldBe` (0,0,0)
    lookupAngVelR c s0 `shouldBe` (0,0,0)

  it "insertRigid then lookup returns stored values" $ do
    let s1 = insertRigid c pos ori vel ang emptyRigid
    lookupPosR    c s1 `shouldBe` pos
    lookupOriR    c s1 `shouldBe` ori
    lookupVelR    c s1 `shouldBe` vel
    lookupAngVelR c s1 `shouldBe` ang
