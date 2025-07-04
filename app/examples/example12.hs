{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Components                (Component(AtomicC))
import           Physics.RigidBody         (InertiaTensor)
import           Physics.RigidState        ( RigidState
                                           , emptyRigid
                                           , insertRigid
                                           , lookupPosR
                                           , lookupVelR
                                           , lookupAngVelR
                                           )
import           Physics.Force3D           (Force3D(..), gravity3D)
import           Physics.Rigid3DNR         ( RRune(..)
                                           , applyRRuneWorld
                                           , driftTrans
                                           , driftRot
                                           , kickForce3D
                                           )
import           Physics.Contact           (contactGround)
import           Physics.LeapfrogNR        (Vec3, vadd, vsub, vscale)
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
import           Data.List                 (intercalate)
import           Text.Printf               (printf)

-- | Cross‐product (LeapfrogNR doesn’t export it)
cross :: Vec3 -> Vec3 -> Vec3
cross (x1,y1,z1) (x2,y2,z2) =
  ( y1*z2 - z1*y2
  , z1*x2 - x1*z2
  , x1*y2 - y1*x2
  )

-- | Solid‐sphere inertia I = (2/5) m r²
sphereInertia :: Double -> Double -> InertiaTensor
sphereInertia m r =
  let i = (2/5) * m * r * r
  in ((i,0,0),(0,i,0),(0,0,i))

-- | Magnus force: F = C·(ω × v), zero torque
magnus3D :: Double -> M.Map Component Double -> Force3D
magnus3D c _massMap = Force3D $ \st cpt ->
  let v = lookupVelR    cpt st
      w = lookupAngVelR cpt st
  in (vscale c (cross w v), (0,0,0))

-- | Build one timestep: kick → driftTrans → driftRot → ground‐bounce
makeStep
  :: [(Component,Double)]
  -> [(Component,InertiaTensor)]
  -> Force3D
  -> [(Component,Double,Double,InertiaTensor)]
  -> RRune
makeStep masses inertias field contactSpecs =
  let kick   = kickForce3D masses inertias field
      driftT = driftTrans    (map fst masses)
      driftR = driftRot      (map fst masses)
      bounce = contactGround 0.9 0.2      contactSpecs  -- e=0.9, μ=0.2
      dom    = S.unions
                [ domainR kick
                , domainR driftT
                , domainR driftR
                , domainR bounce
                ]
      step dt st0 =
        let st1 = applyRRuneWorld kick   dt st0
            st2 = applyRRuneWorld driftT dt st1
            st3 = applyRRuneWorld driftR dt st2
            st4 = applyRRuneWorld bounce dt st3
        in st4
  in RR { domainR = dom, stepR = step }

-- | CSV: time,px,py,pz,vx,vy,vz,ωx,ωy,ωz
formatCSV :: Double -> RigidState -> String
formatCSV t st =
  let (px,py,pz) = lookupPosR    (AtomicC "ball") st
      (vx,vy,vz) = lookupVelR    (AtomicC "ball") st
      (wx,wy,wz) = lookupAngVelR (AtomicC "ball") st
  in intercalate ","
       [ printf "%.4f" t
       , printf "%.4f" px, printf "%.4f" py, printf "%.4f" pz
       , printf "%.4f" vx, printf "%.4f" vy, printf "%.4f" vz
       , printf "%.4f" wx, printf "%.4f" wy, printf "%.4f" wz
       ]

main :: IO ()
main = do
  -- 1) Ball parameters & state
  let r        = 0.02      -- 2 cm radius
      m        = 0.0027    -- 2.7 g
      inertia  = sphereInertia m r
      ball     = AtomicC "ball"

      initSt   = insertRigid ball ( 0,0.10,0 )  -- 10 cm high
                           ( 1,0,0,0 )           -- identity orient.
                           ( 2.0, -3.0, 0.0 )    -- vx=2, vy=-3
                           ( 50.0,  0.0, 0.0 )   -- backspin
                   $ emptyRigid

      masses       = [(ball, m)]
      inertias    = [(ball, inertia)]
      contactSpecs = [(ball, r, m, inertia)]

      -- 2) Forces: gravity + Magnus
      gConst   = 9.81
      magnusC  = 1e-4
      massMap  = M.fromList masses
      gravF    = gravity3D gConst massMap
      magnF    = magnus3D magnusC massMap
      combined = Force3D $ \st cpt ->
        let (f1,τ1) = runForce3D gravF  st cpt
            (f2,τ2) = runForce3D magnF  st cpt
        in (vadd f1 f2, vadd τ1 τ2)

      -- 3) Physics stepper
      stepRune = makeStep masses inertias combined contactSpecs

      -- 4) Simulation parameters
      dt    = 0.001   -- 1 ms
      steps = 2000    -- 2 s

  -- 5) Run & output CSV
  putStrLn "time,px,py,pz,vx,vy,vz,wx,wy,wz"
  let loop 0 _ _  = return ()
      loop n t st = do
        putStrLn (formatCSV t st)
        loop (n-1) (t+dt) (applyRRuneWorld stepRune dt st)

  loop steps 0 initSt
