{-# LANGUAGE TypeApplications #-}

module Physics.ExtraSpec (spec) where

import           Test.Hspec

import           Physics.Forces.Extra
import           Physics.Forces.Force3D         (runForce3D)
import           Physics.RigidBodyUtilities.RigidState
import           Physics.Integrators.LeapfrogNR      (Vec3, vdot)
import qualified Data.Map.Strict         as M
import           Components              (Component (..))

dummyC :: Component
dummyC = AtomicC "dummy"

mkState :: Vec3 -> Vec3 -> RigidState
mkState v w = emptyRigid
  { rsVel    = M.singleton dummyC v
  , rsAngVel = M.singleton dummyC w
  }

shouldBeNear :: Vec3 -> Vec3 -> Expectation
shouldBeNear (ax,ay,az) (bx,by,bz) = do
  (ax `shouldSatisfy` near bx)
  (ay `shouldSatisfy` near by)
  (az `shouldSatisfy` near bz)
  where near a b = abs (a-b) < 1e-9

spec :: Spec
spec = describe "Physics.Force3D.Extra" $ do

  describe "dragQuad3D" $ do

    it "yields zero force when velocity is zero" $ do
      let st        = mkState (0,0,0) (0,0,0)
          (f,_)     = runForce3D (dragQuad3D 1 1 1) st dummyC
      f `shouldBe` (0,0,0)

    it "opposes the velocity direction with expected magnitude" $ do
      let v         = (10,0,0)
          st        = mkState v (0,0,0)
          (f,_)     = runForce3D (dragQuad3D 1 1 1) st dummyC
      f `shouldBe` (-50,0,0)

  describe "magnus3D" $ do

    it "gives zero force when velocity is zero" $ do
      let st        = mkState (0,0,0) (15,2,3)
          (f,_)     = runForce3D (magnus3D 1 0.2 0.02 0.01) st dummyC
      f `shouldBe` (0,0,0)

    it "gives zero force when spin is zero" $ do
      let st        = mkState (3,4,5) (0,0,0)
          (f,_)     = runForce3D (magnus3D 1 0.2 0.02 0.01) st dummyC
      f `shouldBe` (0,0,0)

    it "produces force perpendicular to both v and omega" $ do
      let st        = mkState (10,0,0) (0,0,100)
          (f,_)     = runForce3D (magnus3D 1 1 1 0.5) st dummyC
      f `shouldBeNear` (0,250,0)  
  describe "dragTorque3D" $ do

    it "is proportional and opposite to angular velocity" $ do
      let omega         = (0,0,50)
          st        = mkState (0,0,0) omega
          (_,τ)     = runForce3D (dragTorque3D 2) st dummyC
      τ `shouldBe` (0,0,-100)
