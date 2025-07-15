{-# LANGUAGE RecordWildCards #-}

module Physics.Contact
  ( contactGroundF      -- ^ sphere <-> infinite plane (y = 0)
  , contactSpheresF     -- ^ sphere <-> sphere
  ) where

import           Components              (Component)
import           Data.Set                (Set)
import qualified Data.Set                as S
import qualified Data.Map.Strict         as M
import           Data.List               (foldl', tails)

import           Physics.Math.LinearAlgebra      (Vec3, vadd, vsub, vscale, vdot)
import           Physics.RigidBodyUtilities.RigidState      (RigidState (..))
import           Physics.RigidBodyUtilities.Rigid3DNR       (RRune (..))
import           Physics.Math.LinearAlgebra (invertTensor, applyMat, cross, vnorm, InertiaTensor)

contactGroundF
  :: (Double -> Double)                     -- ^ eFun
  -> (Double -> Double)                     -- ^ muFun
  -> [(Component, Double, Double, InertiaTensor)]
  -> RRune
contactGroundF eFun muFun specs =
  let comps     = [c | (c,_,_,_) <- specs]
      dom       = S.fromList comps
      massMap   = M.fromList [ (c,m) | (c,_,m,_) <- specs ]
      invIMap   = M.fromList [ (c,invertTensor i) | (c,_,_,i) <- specs ]
      radMap    = M.fromList [ (c,r) | (c,r,_,_) <- specs ]

      step _dt st@RigidState{..} =
        let upd = foldl' (stepOne st) (M.empty,M.empty,M.empty) comps
            (posdelta, veldelta, angdelta) = upd

            rsPos'    = M.unionWith vadd rsPos    posdelta
            rsVel'    = M.unionWith vadd rsVel    veldelta
            rsAngVel' = M.unionWith vadd rsAngVel angdelta
        in st { rsPos = rsPos', rsVel = rsVel', rsAngVel = rsAngVel' }

          where
            stepOne st (dpAcc,dvAcc,dwAcc) c =
              let r      = radMap  M.! c
                  m      = massMap M.! c
                  invI   = invIMap M.! c

                  p0     = rsPos M.! c
                  (_,y,_) = p0
                  pen    = r - y     -- >0 ⇒ interpenetration

              in if pen <= 0
                    then (dpAcc,dvAcc,dwAcc)  -- no contact

                    else
                      let n        = (0,1,0)
                          rVec     = (0, -r, 0)

                          vCur     = rsVel M.! c
                          wCur     = rsAngVel M.! c
                          vRel     = vadd vCur (cross wCur rVec)

                          vn       = vdot vRel n          -- normal component (signed)
                          vt       = vsub vRel (vscale vn n)
                          vtMag    = vnorm vt

                          -- coefficient look-ups
                          e        = eFun  (abs vn)
                          mu        = muFun  vtMag

                          -- effective mass for normal
                          invMass  = 1 / m
                          invK     = invMass + vdot rVec (applyMat invI (cross rVec n))
                          
                          -- impulse magnitudes
                          jn       = if vn < 0 && invK > 0
                                       then -(1 + e) * vn / invK
                                       else 0
                          jtMax    = mu * abs jn

                          -- compute tangential impulse limited by Coulomb
                          tDir     = if vtMag < 1e-9 then (0,0,0) else vscale (1/vtMag) vt
                          invKt    = invMass + vdot (cross rVec tDir)
                                               (applyMat invI (cross rVec tDir))
                          jtSlide  = if invKt > 0 then - vtMag / invKt else 0
                          jt       = max (-jtMax) (min jtSlide jtMax)

                          impTotal = vadd (vscale jn n) (vscale jt tDir)

                          -- velocity updates
                          dv       = vscale invMass impTotal
                          dw       = applyMat invI (cross rVec impTotal)

                          -- positional Baumgarte correction
                          beta        = 0.2
                          slop     = 0.01
                          depth    = max 0 (pen - slop)
                          dp       = vscale (beta * depth) n

                          insert v k m = M.insertWith vadd k v m
                      in ( insert dp c dpAcc
                         , insert dv c dvAcc
                         , insert dw c dwAcc )

  in RR { domainR = dom, stepR = step }

contactSpheresF
  :: (Double -> Double)                     -- restitution vn → e
  -> (Double -> Double)                     -- friction     vt → mu
  -> Int                                    -- solver iterations
  -> [(Component, Double, Double, InertiaTensor)]
  -> RRune
contactSpheresF eFun muFun it specs =
  let comps   = [c | (c,_,_,_) <- specs]
      dom     = S.fromList comps
      massMap = M.fromList [ (c,m) | (c,_,m,_) <- specs ]
      invIMap = M.fromList [ (c,invertTensor i) | (c,_,_,i) <- specs ]
      radMap  = M.fromList [ (c,r) | (c,r,_,_) <- specs ]

      step _dt st@RigidState{..} =
        let (dvFinal,dwFinal) =
              iterate impulsePass (zeroMap, zeroMap) !! it

            rsVel'    = M.unionWith vadd rsVel    dvFinal
            rsAngVel' = M.unionWith vadd rsAngVel dwFinal
            rsPos'    = M.unionWith vadd rsPos    posCorr
        in st { rsPos = rsPos', rsVel = rsVel', rsAngVel = rsAngVel' }

          where
            zeroMap = M.fromList [ (c,(0,0,0)) | c <- comps ]

            impulsePass (dvAcc,dwAcc) =
              foldl' solver (dvAcc,dwAcc)
                     [ (c1,c2) | (c1:c2:_) <- tails comps ]

              where
                solver acc@(dvA,dwA) (c1,c2) =
                  let p1    = rsPos M.! c1
                      p2    = rsPos M.! c2
                      d     = vsub p1 p2
                      dist  = vnorm d
                      rSum  = radMap M.! c1 + radMap M.! c2
                  in if dist >= rSum || dist < 1e-9 then acc else
                     let n       = vscale (1/dist) d
                         r1      = vscale (-radMap M.! c1) n
                         r2      = vscale ( radMap M.! c2) n

                         vRel1   = vadd (rsVel M.! c1)
                                        (cross (rsAngVel M.! c1) r1)
                         vRel2   = vadd (rsVel M.! c2)
                                        (cross (rsAngVel M.! c2) r2)
                         vRel    = vsub vRel1 vRel2
                         vn      = vdot vRel n
                         vt      = vsub vRel (vscale vn n)
                         vtMag   = vnorm vt

                         e       = eFun (abs vn)
                         mu       = muFun vtMag

                         invM1   = 1 / massMap M.! c1
                         invM2   = 1 / massMap M.! c2
                         termN1  = vdot n (applyMat (invIMap M.! c1) (cross r1 n))
                         termN2  = vdot n (applyMat (invIMap M.! c2) (cross r2 n))
                         invKn   = invM1 + invM2 + termN1 + termN2

                         jn      | vn < 0 && invKn > 0 = -(1 + e) * vn / invKn
                                 | otherwise           = 0

                         jtMax   = mu * abs jn
                         tDir    = if vtMag < 1e-9 then (0,0,0)
                                   else vscale (1/vtMag) vt
                         termT1  = vdot (cross r1 tDir)
                                         (applyMat (invIMap M.! c1)
                                                   (cross r1 tDir))
                         termT2  = vdot (cross r2 tDir)
                                         (applyMat (invIMap M.! c2)
                                                   (cross r2 tDir))
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

                         insert v k m = M.insertWith vadd k v m
                     in ( insert dv1 c1 $ insert dv2 c2 dvA
                        , insert dw1 c1 $ insert dw2 c2 dwA )

            -- positional correction (Baumgarte)
            beta = 0.2; slop = 0.01
            posCorr = foldl' corrOne M.empty
                        [ (c1,c2) | (c1:c2:_) <- tails comps ]

            corrOne acc (c1,c2) =
              let p1   = rsPos M.! c1
                  p2   = rsPos M.! c2
                  d    = vsub p1 p2
                  dist = vnorm d
                  pen  = radMap M.! c1 + radMap M.! c2 - dist
                  depth = max 0 (pen - slop)
              in if depth <= 0 || dist < 1e-9 then acc else
                 let n       = vscale (1/dist) d
                     invM1   = 1 / massMap M.! c1
                     invM2   = 1 / massMap M.! c2
                     sumInv  = invM1 + invM2
                     c1Off   = vscale (beta*depth * invM1 / sumInv) n
                     c2Off   = vscale (-beta*depth * invM2 / sumInv) n
                     insert v k m = M.insertWith vadd k v m
                 in  insert c1Off c1 $ insert c2Off c2 acc

  in RR { domainR = dom, stepR = step }
