-- 3D Rotation
{-# LANGUAGE RecordWildCards #-}

module Main where

import Physics.RigidState    ( RigidState(..)
                             , emptyRigid
                             , insertRigid
                             )
import Physics.Rigid3DNR
  ( RRune
  , applyRRuneWorld
  , driftTrans
  , driftRot
  , kickForce3D
  )
import Physics.Force3D       ( gravity3D
                             , drag3D
                             , Force3D(..)
                             )
import Components            ( Component(AtomicC) )
import Physics.LeapfrogNR    ( Vec3, vadd )
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import Text.Printf           ( printf )

-- | Our single rigid body
cube :: Component
cube = AtomicC "cube"

-- | Physical parameters
massValue      = 2.0       :: Double       -- kg
inertiaTensor  = ( (1.0,0,0)
                 , (0,1.0,0)
                 , (0,0,1.0)
                 )             :: (Vec3,Vec3,Vec3)  -- simple unit cube

-- | Build the composite Force3D: gravity downward + drag
forceField :: Force3D
forceField =
  let mmap      = M.fromList [(cube,massValue)]
      gField    = gravity3D 9.81 mmap
      dField    = drag3D    0.5    -- drag coefficient
  in Force3D $ \st c ->
       let (fG,τG) = runForce3D gField st c
           (fD,τD) = runForce3D dField st c
       in ( vadd fG fD
          , vadd τG τD
          )

-- | The three runes for our integrator
driftRune :: RRune
driftRune = driftTrans [cube]

rotRune   :: RRune
rotRune   = driftRot   [cube]

kickRune  :: RRune
kickRune  = kickForce3D
              [(cube,massValue)]
              [(cube,inertiaTensor)]
              forceField

-- | One full step: drift → rotate → kick
stepRigid :: Double -> RigidState -> RigidState
stepRigid dt st0 =
  let st1 = applyRRuneWorld driftRune dt st0
      st2 = applyRRuneWorld rotRune   dt st1
  in  applyRRuneWorld kickRune  dt st2

-- | Initial state: at origin, no rotation, small initial spin
initialState :: RigidState
initialState =
  insertRigid cube
    (0,0,0)         -- position
    (1,0,0,0)       -- quaternion (w=1 identity)
    (1,0,0)         -- linear velocity (m/s)
    (0,1,0)         -- angular velocity (rad/s)
    emptyRigid

-- | Run N steps and collect the trajectory
simulate :: Int -> Double -> RigidState -> [RigidState]
simulate n dt = take (n+1) . iterate (stepRigid dt)

-- | Print time, position, and quaternion
printStep :: Double -> RigidState -> IO ()
printStep t RigidState{..} =
  let (x,y,z) = rsPos M.! cube
      (qw,qx,qy,qz) = rsOri M.! cube
  in printf "%.3f: pos=(%.3f,%.3f,%.3f)  ori=(%.3f,%.3f,%.3f,%.3f)\n"
            t x y z qw qx qy qz

main :: IO ()
main = do
  let dt   = 0.02    -- 20 ms timestep
      steps = 200    -- simulate 4 seconds
      traj  = simulate steps dt initialState
  mapM_ (uncurry printStep) (zip [0,dt..] traj)
