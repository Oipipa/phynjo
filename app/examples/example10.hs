{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.List              (intercalate)
import           Text.Printf            (printf)
import qualified Data.Map.Strict        as M

import           Components             (Component(AtomicC))
import           Physics.RigidBody      (InertiaTensor)
import           Physics.RigidState     ( RigidState
                                        , emptyRigid
                                        , insertRigid
                                        , lookupPosR
                                        , lookupOriR
                                        , lookupVelR
                                        , lookupAngVelR
                                        )
import           Physics.Force3D        (Force3D(..))
import           Physics.Rigid3DNR      ( RRune(..)
                                        , applyRRuneWorld
                                        , driftTrans
                                        , driftRot
                                        , kickForce3D
                                        )

-- | Build a Force3D that applies gravity at COM and torque about a fixed pivot
gravityWithPivot
  :: Double                    -- ^ g > 0
  -> (Double,Double,Double)    -- ^ pivot point
  -> M.Map Component Double    -- ^ mass map
  -> Force3D
gravityWithPivot g pivot massMap = Force3D $ \st c ->
  let m      = massMap M.! c
      -- linear force straight down
      f@(fx,fy,fz) = (0, - m * g, 0)
      -- lever arm from pivot to COM
      (x,y,z)      = lookupPosR c st
      (px,py,pz)   = pivot
      lx = x - px; ly = y - py; lz = z - pz
      -- torque = lever × F
      τx = ly*fz - lz*fy
      τy = lz*fx - lx*fz
      τz = lx*fy - ly*fx
  in (f, (τx, τy, τz))

makeStep
  :: [(Component,Double)] 
  -> [(Component,InertiaTensor)] 
  -> Force3D 
  -> RRune
makeStep masses inertias field =
  let kick   = kickForce3D masses inertias field
      driftT = driftTrans (map fst masses)
      driftR = driftRot   (map fst masses)
      dom    = domainR kick
      step dt st0 =
        let st1 = applyRRuneWorld kick   dt st0
            st2 = applyRRuneWorld driftT dt st1
        in  applyRRuneWorld driftR dt st2
  in RR { domainR = dom, stepR = step }

-- | Simulate for n steps, returning list of (time, state)
simulate
  :: RRune
  -> RigidState
  -> Double        -- ^ dt
  -> Int           -- ^ n steps
  -> [(Double, RigidState)]
simulate rune initSt dt n =
  let go 0 t st = [(t,st)]
      go k t st =
        (t,st) : go (k-1) (t+dt) (applyRRuneWorld rune dt st)
  in go n 0 initSt

-- | CSV‐format one sample for a given component
formatCSV :: Component -> (Double, RigidState) -> String
formatCSV comp (t,st) =
  let (px,py,pz)    = lookupPosR    comp st
      (qw,qx,qy,qz) = lookupOriR    comp st
      (vx,vy,vz)    = lookupVelR    comp st
      (wx,wy,wz)    = lookupAngVelR comp st
      fields = [ printf "%.6f" t
               , printf "%.6f" px, printf "%.6f" py, printf "%.6f" pz
               , printf "%.6f" qw, printf "%.6f" qx
               , printf "%.6f" qy, printf "%.6f" qz
               , printf "%.6f" vx, printf "%.6f" vy, printf "%.6f" vz
               , printf "%.6f" wx, printf "%.6f" wy, printf "%.6f" wz
               ]
  in intercalate "," fields

main :: IO ()
main = do
  -- 1) Define the gyroscope body
  let comp     = AtomicC "gyro"
      mass     = 1.0
      inertia  :: InertiaTensor
      inertia  = ((0.5,0,0),(0,0.25,0),(0,0,0.25))
      pos0     = (0, 0.1, 0)
      ori0     = (1,0,0,0)
      vel0     = (0,0,0)
      angVel0  = (50,0,0)

      -- initial RigidState
      initState = insertRigid comp pos0 ori0 vel0 angVel0 emptyRigid

      masses    = [(comp, mass)]
      inertias = [(comp, inertia)]
      massMap   = M.fromList masses

      -- 2) Build gravity+pivot field (pivot at origin)
      g         = 9.81
      pivotPt   = (0,0,0)
      field     = gravityWithPivot g pivotPt massMap

      -- 3) One combined RRune
      stepRune  = makeStep masses inertias field

      -- 4) Simulation parameters
      dt      = 0.001   -- 1 ms
      steps   = 20000  -- 20 s

      -- 5) Generate trajectory
      traj    = simulate stepRune initState dt steps

  -- 6) Emit CSV header + rows
  putStrLn $ intercalate ","
    [ "time"
    , "posX","posY","posZ"
    , "oriW","oriX","oriY","oriZ"
    , "velX","velY","velZ"
    , "angX","angY","angZ"
    ]
  mapM_ (putStrLn . formatCSV comp) traj
