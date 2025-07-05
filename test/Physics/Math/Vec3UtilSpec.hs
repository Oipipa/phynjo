module Physics.Math.Vec3UtilSpec (spec) where

import           Test.Hspec
import qualified Physics.Math.Vec3Util as V

spec :: Spec
spec = describe "Physics.Math.Vec3Util" $ do

  it "adds and subtracts component-wise" $ do
    let a = (1,2,3)
        b = (4,5,6)
    V.vadd a b `shouldBe` (5,7,9)
    V.vsub b a `shouldBe` (3,3,3)

  it "scales correctly" $ do
    V.vscale 2 (1,2,-3) `shouldBe` (2,4,-6)

  it "dot product matches hand calculation" $ do
    V.vdot (1,2,3) (4,5,6) `shouldBe` 32   -- 1*4+2*5+3*6

  it "norm squared equals dot v·v" $ do
    let v = (2, -3, 6)
    V.vnorm2 v `shouldBe` V.vdot v v

  it "hat returns a unit vector and keeps direction" $ do
    let v = (3, 0, 4)          -- length 5
        u = V.vhat v
    V.vnorm u `shouldSatisfy` (\n -> abs (n - 1) < 1e-12)
    -- direction preserved (parallel)
    V.vdot u v `shouldSatisfy` (> 0)

  it "hat of zero gives zero (no NaNs)" $ do
    V.vhat V.vzero `shouldBe` V.vzero

  it "approxEqVec recognises near-equality" $ do
    let a = (1e-10, -1e-10, 0)
    V.approxEqVec a V.vzero `shouldBe` True
