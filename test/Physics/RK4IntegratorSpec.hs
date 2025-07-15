{-# LANGUAGE RecordWildCards #-}
module Physics.RK4IntegratorSpec (spec) where

import Test.Hspec
import Physics.Integrators.RK4Integrator (rk4Step, integrateRK4)
import Physics.Integrators.LeapfrogNR   (State(..), MassMap)
import Physics.Math.LinearAlgebra   (Vec3)
import qualified Data.Map.Strict as M

spec :: Spec
spec = describe "Physics.RK4Integrator" $ do

  let h      = 0.1
      g      = 1.0
      masses :: MassMap
      masses = M.fromList [(1,1.0),(2,1.0)]
      zeroV  = (0,0,0) :: Vec3

      state0 :: State
      state0 = State
        { pos = M.fromList [(1,(0,0,0)),(2,(2,0,0))]
        , vel = M.fromList [(1,zeroV),(2,zeroV)]
        }

  it "rk4Step with h=0 leaves state unchanged" $ do
    rk4Step 0 g masses state0 `shouldBe` state0

  it "single-body self-gravity is zero acceleration" $ do
    let masses1 = M.fromList [(1,1.0)]
        st1 = State { pos = M.singleton 1 (0,0,0)
                    , vel = M.singleton 1 zeroV }
    rk4Step h g masses1 st1 `shouldBe` st1

  it "integrateRK4 0 steps returns initial state only" $ do
    integrateRK4 0 h g masses state0 `shouldBe` [state0]
