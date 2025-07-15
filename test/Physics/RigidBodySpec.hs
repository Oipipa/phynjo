{-# LANGUAGE OverloadedStrings #-}

module Physics.RigidBodySpec (spec) where

import Test.Hspec
import Components            (Component(AtomicC))
import Physics.RigidBodyUtilities.RigidBody
import Physics.Math.LinearAlgebra    (Vec3)

spec :: Spec
spec = describe "Physics.RigidBody.mkRigidBody" $ do
  let ident    = AtomicC "foo"
      mass     = 2.5
      inertia  = ((1,0,0),(0,1,0),(0,0,1))  :: InertiaTensor
      pos      = (0.0, 1.0, 2.0)           :: Vec3
      ori      = (1.0, 0.0, 0.0, 0.0)      :: Quaternion
      vel      = (3.0, 4.0, 5.0)           :: Vec3
      angVel   = (0.1, 0.2, 0.3)           :: Vec3
      rb       = mkRigidBody ident mass inertia pos ori vel angVel

  it "sets the identifier" $
    rbIdent rb `shouldBe` ident

  it "sets the mass" $
    rbMass rb `shouldBe` mass

  it "sets the inertia tensor" $
    rbInertia rb `shouldBe` inertia

  it "sets the initial position" $
    rbPos0 rb `shouldBe` pos

  it "sets the initial orientation" $
    rbOri0 rb `shouldBe` ori

  it "sets the initial linear velocity" $
    rbVel0 rb `shouldBe` vel

  it "sets the initial angular velocity" $
    rbAngVel0 rb `shouldBe` angVel
