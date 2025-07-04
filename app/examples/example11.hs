{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.List                 (intercalate)
import           Text.Printf               (printf)
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as Set

import           Components                (Component(AtomicC))
import           Physics.RigidBody         (InertiaTensor)
import           Physics.RigidState        ( RigidState
                                           , emptyRigid
                                           , insertRigid
                                           , lookupPosR
                                           )
import           Physics.Rigid3DNR         ( RRune(..)
                                           , applyRRuneWorld
                                           , driftTrans
                                           )
import           Physics.Contact           (contactSpheres)

-- | Solid sphere inertia: I = (2/5)·m·r²
sphereInertia :: Double -> Double -> InertiaTensor
sphereInertia m r =
  let i = (2/5) * m * r * r
  in ((i,0,0),(0,i,0),(0,0,i))

-- | Build one time‐step:
--    1) Drift half‐step
--    2) Resolve collisions (full dt)
--    3) Drift half‐step
makeStep
  :: [(Component,Double,Double,InertiaTensor)]  -- specs
  -> Double                                      -- restitution
  -> Double                                      -- friction
  -> Int                                         -- solver iterations
  -> RRune
makeStep specs e μ it =
  let comps    = [c | (c,_,_,_) <- specs]
      -- half‐drift and collision runes
      drift    = driftTrans comps
      collide  = contactSpheres e μ it specs
      -- union of domains
      dom      = Set.union (domainR drift) (domainR collide)

      step dt st0 =
        let half    = dt/2
            st1     = applyRRuneWorld drift   half st0
            st2     = applyRRuneWorld collide dt   st1
            st3     = applyRRuneWorld drift   half st2
        in st3
  in RR { domainR = dom, stepR = step }

-- | Print CSV: time,x1,y1,z1,x2,y2,z2
formatCSV :: Double -> RigidState -> String
formatCSV t st =
  let (x1,y1,z1) = lookupPosR (AtomicC "s1") st
      (x2,y2,z2) = lookupPosR (AtomicC "s2") st
  in intercalate ","
       [ printf "%.4f" t
       , printf "%.4f" x1, printf "%.4f" y1, printf "%.4f" z1
       , printf "%.4f" x2, printf "%.4f" y2, printf "%.4f" z2
       ]

main :: IO ()
main = do
  -- 1) Sphere parameters
  let r      = 0.5
      m      = 1.0
      inertia= sphereInertia m r

      c1     = AtomicC "s1"
      c2     = AtomicC "s2"

      -- 2) Initial state: s1 at x=–1 moving right; s2 at x=+1 moving left
      initSt = insertRigid c2 ( 1,0,0) (1,0,0,0) (-1,0,0) (0,0,0)
             $ insertRigid c1 (-1,0,0) (1,0,0,0) ( 1,0,0) (0,0,0)
             $ emptyRigid

      specs      =
        [ (c1, r, m, inertia)
        , (c2, r, m, inertia)
        ]

      restitution = 1.0    -- perfectly elastic
      friction    = 0.0    -- no friction
      iterations  = 1      -- one pass is enough here

      -- 3) Build our Strang‐split collision step
      stepRune   = makeStep specs restitution friction iterations

      -- 4) Simulation parameters
      dt    = 0.01
      steps = 200

  -- 5) Run and print CSV
  putStrLn "time,x1,y1,z1,x2,y2,z2"
  let loop 0 _ _ = return ()
      loop n t st = do
        putStrLn (formatCSV t st)
        let st' = applyRRuneWorld stepRune dt st
        loop (n-1) (t+dt) st'
  loop steps 0 initSt
