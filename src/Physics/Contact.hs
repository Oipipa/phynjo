{-# LANGUAGE RecordWildCards #-}

module Physics.Contact
  ( contactGroundF
  , contactSpheresF 
  ) where

import           Components                              (Component)
import           Data.Set                                (Set)
import qualified Data.Set                                as S
import qualified Data.Map.Strict                         as M
import           Data.List                               (foldl', tails)

import           Physics.Math.LinearAlgebra              (Vec3, vadd, vsub, vscale, vdot, cross, vnorm
                                                        , InertiaTensor, applyMat, invertTensor)
import           Physics.RigidBodyUtilities.RigidState   (RigidState (..))
import           Physics.RigidBodyUtilities.Rigid3DNR    (RRune (..))

pairs :: [a] -> [(a,a)]
pairs xs = [ (x,y) | (x:rest) <- tails xs, y <- rest ]

insertAccum :: (Ord k) => (Vec3 -> Vec3 -> Vec3) -> k -> Vec3 -> M.Map k Vec3 -> M.Map k Vec3
insertAccum add k v = M.insertWith add k v

contactGroundF
  :: (Double -> Double)                     -- restitution as a function of |vn|
  -> (Double -> Double)                     -- friction as a function of |vt|
  -> [(Component, Double, Double, InertiaTensor)]  -- (id, radius, mass, inertia)
  -> RRune
contactGroundF eFun muFun specs =
  let comps     = [c | (c,_,_,_) <- specs]
      dom       = S.fromList comps
      massMap   = M.fromList [ (c,m) | (c,_,m,_) <- specs ]
      invIMap   = M.fromList [ (c,invertTensor i) | (c,_,_,i) <- specs ]
      radMap    = M.fromList [ (c,r) | (c,r,_,_) <- specs ]

      step _dt st@RigidState{..} =
        let (posdelta, veldelta, angdelta) =
              foldl' (stepOne st) (M.empty, M.empty, M.empty) comps

            rsPos'    = M.unionWith vadd rsPos    posdelta
            rsVel'    = M.unionWith vadd rsVel    veldelta
            rsAngVel' = M.unionWith vadd rsAngVel angdelta
        in st { rsPos = rsPos', rsVel = rsVel', rsAngVel = rsAngVel' }

        where
          stepOne _st (dpAcc,dvAcc,dwAcc) c =
            let r        = radMap  M.! c
                m        = massMap M.! c
                invI     = invIMap M.! c

                p0       = rsPos M.! c
                (_,y,_)  = p0
                pen      = r - y         -- > 0 means penetration

            in if pen <= 0
                 then (dpAcc,dvAcc,dwAcc)
                 else
                   let n        = (0,1,0)          -- up
                       rVec     = (0, -r, 0)       -- contact point from COM

                       vCur     = rsVel    M.! c
                       wCur     = rsAngVel M.! c
                       vRel     = vadd vCur (cross wCur rVec)

                       vn       = vdot vRel n
                       vt       = vsub vRel (vscale vn n)
                       vtMag    = vnorm vt

                       e        = eFun (abs vn)
                       mu       = muFun vtMag

                       invMass  = 1 / m
                       -- correct normal effective mass:
                       termN    = vdot (cross rVec n)
                                         (applyMat invI (cross rVec n))
                       invKn    = invMass + termN

                       jn       = if vn < 0 && invKn > 0
                                   then -(1 + e) * vn / invKn else 0

                       jtMax    = mu * abs jn
                       tDir     = if vtMag < 1e-9 then (0,0,0)
                                  else vscale (1/vtMag) vt
                       termT    = vdot (cross rVec tDir)
                                         (applyMat invI (cross rVec tDir))
                       invKt    = invMass + termT
                       jtSlide  = if invKt > 0 then - vtMag / invKt else 0
                       jt       = max (-jtMax) (min jtSlide jtMax)

                       impTotal = vadd (vscale jn n) (vscale jt tDir)

                       dv       = vscale invMass impTotal
                       dw       = applyMat invI (cross rVec impTotal)

                       -- positional Baumgarte correction
                       beta     = 0.2
                       slop     = 0.01
                       depth    = max 0 (pen - slop)
                       dp       = vscale (beta * depth) n
                   in ( insertAccum vadd c dp dpAcc
                      , insertAccum vadd c dv dvAcc
                      , insertAccum vadd c dw dwAcc )

  in RR { domainR = dom, stepR = step }


contactSpheresF
  :: (Double -> Double) 
  -> (Double -> Double) 
  -> Int
  -> [(Component, Double, Double, InertiaTensor)]  -- (id, radius, mass, inertia)
  -> RRune
contactSpheresF eFun muFun it specs =
  let comps   = [c | (c,_,_,_) <- specs]
      dom     = S.fromList comps
      massMap = M.fromList [ (c,m) | (c,_,m,_) <- specs ]
      invIMap = M.fromList [ (c,invertTensor i) | (c,_,_,i) <- specs ]
      radMap  = M.fromList [ (c,r) | (c,r,_,_) <- specs ]

      step _dt st@RigidState{..} =
        let zeroMap = M.fromList [ (c,(0,0,0)) | c <- comps ]

            -- Gauss–Seidel style: each pass uses accumulated dv/dw
            impulsePass (dvAcc,dwAcc) =
              foldl' solver (dvAcc,dwAcc) (pairs comps)
              where
                solver acc@(dvA,dwA) (c1,c2) =
                  let p1    = rsPos M.! c1
                      p2    = rsPos M.! c2
                      d     = vsub p1 p2
                      dist  = vnorm d
                      rSum  = (radMap M.! c1) + (radMap M.! c2)
                  in if dist >= rSum || dist < 1e-9
                        then acc
                        else
                          let n       = vscale (1/dist) d
                              r1      = vscale (-radMap M.! c1) n
                              r2      = vscale ( radMap M.! c2) n

                              -- use accumulated velocities
                              v1      = vadd (rsVel    M.! c1)
                                             (M.findWithDefault (0,0,0) c1 dvA)
                              w1      = vadd (rsAngVel M.! c1)
                                             (M.findWithDefault (0,0,0) c1 dwA)
                              v2      = vadd (rsVel    M.! c2)
                                             (M.findWithDefault (0,0,0) c2 dvA)
                              w2      = vadd (rsAngVel M.! c2)
                                             (M.findWithDefault (0,0,0) c2 dwA)

                              vRel1   = vadd v1 (cross w1 r1)
                              vRel2   = vadd v2 (cross w2 r2)
                              vRel    = vsub vRel1 vRel2
                              vn      = vdot vRel n
                              vt      = vsub vRel (vscale vn n)
                              vtMag   = vnorm vt

                              e       = eFun (abs vn)
                              mu      = muFun vtMag

                              invM1   = 1 / (massMap M.! c1)
                              invM2   = 1 / (massMap M.! c2)

                              -- correct normal effective mass terms
                              termN1  = vdot (cross r1 n)
                                               (applyMat (invIMap M.! c1) (cross r1 n))
                              termN2  = vdot (cross r2 n)
                                               (applyMat (invIMap M.! c2) (cross r2 n))
                              invKn   = invM1 + invM2 + termN1 + termN2

                              jn      | vn < 0 && invKn > 0 = -(1 + e) * vn / invKn
                                      | otherwise           = 0

                              jtMax   = mu * abs jn

                              tDir    = if vtMag < 1e-9 then (0,0,0)
                                        else vscale (1/vtMag) vt
                              termT1  = vdot (cross r1 tDir)
                                               (applyMat (invIMap M.! c1) (cross r1 tDir))
                              termT2  = vdot (cross r2 tDir)
                                               (applyMat (invIMap M.! c2) (cross r2 tDir))
                              invKt   = invM1 + invM2 + termT1 + termT2
                              jtSlide | invKt > 0 = - vtMag / invKt
                                      | otherwise = 0
                              jt      = max (-jtMax) (min jtSlide jtMax)

                              impN    = vscale jn n
                              impT    = vscale jt tDir
                              imp1    = vadd impN impT
                              imp2    = vscale (-1) imp1

                              dv1     = vscale invM1 imp1
                              dv2     = vscale invM2 imp2
                              dw1     = applyMat (invIMap M.! c1) (cross r1 imp1)
                              dw2     = applyMat (invIMap M.! c2) (cross r2 imp2)
                          in  ( insertAccum vadd c1 dv1 $ insertAccum vadd c2 dv2 dvA
                              , insertAccum vadd c1 dw1 $ insertAccum vadd c2 dw2 dwA )

            -- perform it iterations
            (dvFinal,dwFinal) = iterate impulsePass (zeroMap, zeroMap) !! max 0 it

            -- positional correction (Baumgarte) using all pairs
            beta = 0.2
            slop = 0.01
            posCorr = foldl' corrOne M.empty (pairs comps)
              where
                corrOne acc (c1,c2) =
                  let p1    = rsPos M.! c1
                      p2    = rsPos M.! c2
                      d     = vsub p1 p2
                      dist  = vnorm d
                      pen   = (radMap M.! c1) + (radMap M.! c2) - dist
                      depth = max 0 (pen - slop)
                  in if depth <= 0 || dist < 1e-9
                        then acc
                        else
                          let n       = vscale (1/dist) d
                              invM1   = 1 / (massMap M.! c1)
                              invM2   = 1 / (massMap M.! c2)
                              sumInv  = invM1 + invM2
                              c1Off   = vscale (beta*depth * invM1 / sumInv) n
                              c2Off   = vscale (-beta*depth * invM2 / sumInv) n
                          in  insertAccum vadd c1 c1Off
                           $  insertAccum vadd c2 c2Off acc

            rsVel'    = M.unionWith vadd rsVel    dvFinal
            rsAngVel' = M.unionWith vadd rsAngVel dwFinal
            rsPos'    = M.unionWith vadd rsPos    posCorr
        in st { rsPos = rsPos', rsVel = rsVel', rsAngVel = rsAngVel' }

  in RR { domainR = dom, stepR = step }
