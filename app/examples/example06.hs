{-# LANGUAGE RecordWildCards #-}

module Main where

import Physics.RigidBodyUtilities.RigidState    ( RigidState(..)
                             , emptyRigid
                             , insertRigid
                             )
import Physics.RigidBodyUtilities.Rigid3DNR
                            ( RRune
                            , applyRRuneWorld
                            , driftTrans
                            , kickForce3D
                            )

import Physics.Forces.Force3D       ( gravity3D, Force3D(..) )
import Components            ( Component(AtomicC) )
import qualified Data.Map.Strict as M
import Text.Printf           ( printf )

-- our “particle” component
proj :: Component
proj = AtomicC "proj"

-- physical constants
massVal    :: Double
massVal    = 1.0       -- kg

gAccel     :: Double
gAccel     = 9.81      -- m/s² downward (–Y)

-- we need a dummy inertia tensor (no rotation)
-- use identity 3×3 so that angular updates do nothing
inertiaId :: ((Double,Double,Double),
              (Double,Double,Double),
              (Double,Double,Double))
inertiaId = ((1,0,0),(0,1,0),(0,0,1))

-- force field: pure gravity, no torque
forceField :: Force3D
forceField =
  let massMap = M.singleton proj massVal
      gF      = gravity3D gAccel massMap
  in Force3D $ \st c ->
       if c == proj then runForce3D gF st c else ((0,0,0),(0,0,0))

-- runes: drift only in translation, then kick under gravity
driftR :: RRune
driftR = driftTrans [proj]

kickR  :: RRune
kickR  = kickForce3D
           [(proj, massVal)]
           [(proj, inertiaId)]
           forceField

-- simple “velocity‐Verlet” step: half‐drift, full‐kick, half‐drift
step2D :: Double -> RigidState -> RigidState
step2D dt st0 =
  let h = dt/2
      st1 = applyRRuneWorld driftR h st0
      st2 = applyRRuneWorld kickR  dt st1
  in      applyRRuneWorld driftR h st2

-- initial state: launch from origin at 45° with speed v0
initialState :: Double -> RigidState
initialState v0 =
  insertRigid proj
    (0,0,0)         -- start at (x=0,y=0)
    (1,0,0,0)       -- quaternion identity
    (vx, vy, 0)     -- linear vel (vx,vy,0)
    (0,0,0)         -- angular vel zero
    emptyRigid
  where
    vx = v0 / sqrt 2
    vy = v0 / sqrt 2

-- energy: KE + PE
totalEnergy :: Double -> RigidState -> Double
totalEnergy g st =
  let (vx,vy,_) = rsVel st M.! proj
      (_, y, _) = rsPos st M.! proj
      ke = 0.5 * massVal * (vx*vx + vy*vy)
      pe = massVal * g * y
  in ke + pe

main :: IO ()
main = do
  let v0     = 20.0    -- m/s
      dt     = 0.01    -- 10 ms timestep
      steps  = 2000    -- simulate 20 s
      traj   = iterate (step2D dt) (initialState v0)
      times  = [0, dt ..]
  -- print CSV header
  putStrLn "t,x,y,E"
  -- take only until projectile returns to y≤0
  let results = takeWhile (\st ->
                    let (_,y,_) = rsPos st M.! proj
                    in y >= 0) traj
  mapM_ (\(t,st) ->
           let (x, y, _) = rsPos st M.! proj
               e         = totalEnergy gAccel st
           in printf "%.3f,%.6f,%.6f,%.6f\n" t x y e
        ) (zip times results)
