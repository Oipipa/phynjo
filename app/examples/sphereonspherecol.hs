{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Set           as S

import Phynjo.Core 
import Phynjo.RB 
import Phynjo.Collisions

sphereInertia :: Double -> Double -> InertiaTensor
sphereInertia m r =
  let i = (2/5) * m * r * r
  in ((i,0,0),(0,i,0),(0,0,i))

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
  let r           = 0.5
      m           = 1.0
      inertia     = sphereInertia m r
      c1          = AtomicC "s1"
      c2          = AtomicC "s2"
      comps       = [c1,c2]
      initSt      = insertRigid c2 ( 1,0,0) (1,0,0,0) (-1,0,0) (0,0,0)
                  $ insertRigid c1 (-1,0,0) (1,0,0,0) ( 1,0,0) (0,0,0)
                  $ emptyRigid
      specs       = [(c1,r,m,inertia),(c2,r,m,inertia)]
      shapes      = [(c1,ShSphere r),(c2,ShSphere r)]
      gridSize    = 2*r
      restitution = 1.0
      friction    = 0.0
      iterations  = 1
      driftRule   = driftTrans comps
      dt          = 0.01
      steps       = 200

  putStrLn "time,x1,y1,z1,x2,y2,z2"
  let loop 0 _ _ = return ()
      loop n t st = do
        putStrLn (formatCSV t st)
        let half = dt/2
            bbs  = [ (c, BB_AABB (aabbFromSphere (SphereBB (lookupPosR c st) r)))
                   | (c,ShSphere _) <- shapes
                   ]
            cm0   = buildManager (BP_Grid gridSize) bbs
            cm1   = updateManager cm0 bbs
            pairs = runBroadPhase cm1
            col   = narrowPhase (const restitution) (const friction) iterations shapes pairs
            st1   = applyRRuneWorld driftRule half st
            st2   = applyRRuneWorld col        dt   st1
            st3   = applyRRuneWorld driftRule half st2
        loop (n-1) (t+dt) st3

  loop steps 0 initSt