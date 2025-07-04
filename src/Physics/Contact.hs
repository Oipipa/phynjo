{-# LANGUAGE RecordWildCards #-}

module Physics.Contact
  ( contactGround
  , contactSpheres
  ) where

import           Components             (Component)
import           Physics.Rigid3DNR      (RRune(..))
import           Physics.RigidState     (RigidState(..))
import           Physics.LeapfrogNR     (Vec3, vadd, vsub, vscale, vdot)
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S
import           Data.List              (foldl', tails)

-- | A 3×3 inertia tensor
type InertiaTensor = (Vec3, Vec3, Vec3)

-- | Cofactor‐based inversion of a 3×3 tensor
invertTensor :: InertiaTensor -> InertiaTensor
invertTensor ((a,b,c),(d,e,f),(g,h,i)) =
  let det    = a*(e*i - f*h) - b*(d*i - f*g) + c*(d*h - e*g)
      invDet = 1 / det
      c11 =  (e*i - f*h); c12 = -(d*i - f*g); c13 =  (d*h - e*g)
      c21 = -(b*i - c*h); c22 =  (a*i - c*g); c23 = -(a*h - b*g)
      c31 =  (b*f - c*e); c32 = -(a*f - c*d); c33 =  (a*e - b*d)
  in ( (c11*invDet, c12*invDet, c13*invDet)
     , (c21*invDet, c22*invDet, c23*invDet)
     , (c31*invDet, c32*invDet, c33*invDet)
     )

-- | Matrix‐vector multiply
applyMat :: InertiaTensor -> Vec3 -> Vec3
applyMat ((a,b,c),(d,e,f),(g,h,i)) (x,y,z) =
  ( a*x + b*y + c*z
  , d*x + e*y + f*z
  , g*x + h*y + i*z
  )

-- | Cross‐product
cross :: Vec3 -> Vec3 -> Vec3
cross (x1,y1,z1) (x2,y2,z2) =
  ( y1*z2 - z1*y2
  , z1*x2 - x1*z2
  , x1*y2 - y1*x2
  )

-- | Euclidean norm
vnorm :: Vec3 -> Double
vnorm v = sqrt (vdot v v)

--------------------------------------------------------------------------------
-- 1) Ground contact: one‐pass impulse + Baumgarte push‐out against y=0.

contactGround
  :: Double                             -- restitution e ∈ [0,1]
  -> Double                             -- friction μ ≥ 0
  -> [(Component, Double, Double, InertiaTensor)]
  -> RRune
contactGround e μ specs =
  let bodies   = [c | (c,_,_,_) <- specs]
      dom      = S.fromList bodies
      massMap  = M.fromList [ (c,m)    | (c,_,m,_) <- specs ]
      invIMap  = M.fromList [ (c,invertTensor i)
                            | (c,_,_,i) <- specs ]
      radii    = M.fromList [ (c,r)    | (c,r,_,_) <- specs ]

      step _dt st0 =
        let pos0 = rsPos    st0
            vel0 = rsVel    st0
            ang0 = rsAngVel st0

            -- one impulse pass
            (dvF, dwF) =
              foldl' (\(dvA,dwA) c ->
                        let r      = radii   M.! c
                            m      = massMap  M.! c
                            invI   = invIMap  M.! c
                            (_,y,_) = pos0   M.! c
                            pen    = r - y
                        in if pen > 0
                              then
                                let n      = (0,1,0)
                                    rVec   = (0,-r,0)
                                    vRel   = vadd (vel0 M.! c)
                                                  (cross (ang0 M.! c) rVec)
                                    vN     = vdot vRel n
                                in if vN < 0
                                      then
                                        -- normal impulse
                                        let jn    = -(1 + e) * vN * m
                                            impN  = vscale jn n

                                            -- tangential relative velocity
                                            vt    = vsub vRel (vscale vN n)
                                            vtMag = vnorm vt
                                            tDir  = if vtMag == 0
                                                    then (0,0,0)
                                                    else vscale (1/vtMag) vt

                                            -- friction: effective tangential mass
                                            jtMax = μ * abs jn
                                            invMt = (1/m)
                                                  + vdot (cross rVec tDir)
                                                         (applyMat invI (cross rVec tDir))
                                            mt    = if invMt /= 0 then 1 / invMt else 0
                                            jt    = min (mt * vtMag) jtMax
                                            impT  = vscale (-jt) tDir

                                            total = vadd impN impT
                                            dv    = vscale (1/m) total
                                            dw    = applyMat invI (cross rVec total)
                                        in ( M.insertWith vadd c dv dvA
                                           , M.insertWith vadd c dw dwA
                                           )
                                      else (dvA,dwA)
                              else (dvA,dwA)
                     ) (M.empty, M.empty) bodies

            -- apply velocity impulses
            vel' = M.mapWithKey
                     (\c v -> vadd v (M.findWithDefault (0,0,0) c dvF))
                     vel0
            ang' = M.mapWithKey
                     (\c w -> vadd w (M.findWithDefault (0,0,0) c dwF))
                     ang0

            -- positional (Baumgarte) correction
            β    = 0.2
            slop = 0.01
            corr =
              foldl' (\acc c ->
                        let r       = radii   M.! c
                            (_,y,_) = pos0    M.! c
                            depth   = max 0 (r - y - slop)
                        in if depth > 0
                              then
                                let n = (0,1,0)
                                    δ = vscale (β * depth) n
                                in M.insertWith vadd c δ acc
                              else acc
                     ) M.empty bodies

            pos' = M.mapWithKey
                     (\c p -> vadd p (M.findWithDefault (0,0,0) c corr))
                     pos0

        in st0 { rsPos    = pos'
               , rsVel    = vel'
               , rsAngVel = ang' }

  in RR { domainR = dom, stepR = step }

--------------------------------------------------------------------------------
-- 2) Sphere↔sphere contacts with corrected friction and push‐out.

contactSpheres
  :: Double                             -- restitution e
  -> Double                             -- friction μ
  -> Int                                -- iterations
  -> [(Component, Double, Double, InertiaTensor)]
  -> RRune
contactSpheres e μ it specs =
  let bodies   = [c | (c,_,_,_) <- specs]
      dom      = S.fromList bodies
      massMap  = M.fromList [ (c,m)    | (c,_,m,_) <- specs ]
      invIMap  = M.fromList [ (c,invertTensor i)
                            | (c,_,_,i) <- specs ]
      radMap   = M.fromList [ (c,r)    | (c,r,_,_) <- specs ]

      step _dt st0 =
        let pos0 = rsPos    st0
            vel0 = rsVel    st0
            ang0 = rsAngVel st0

            -- init impulse accumulators
            dv0  = M.fromList [ (c,(0,0,0)) | c <- bodies ]
            dw0  = M.fromList [ (c,(0,0,0)) | c <- bodies ]

            -- one pass of impulse resolution
            onePass (dvA,dwA) =
              foldl' (\(dvAcc,dwAcc) (c1,c2) ->
                        let p1    = pos0 M.! c1
                            p2    = pos0 M.! c2
                            d     = vsub p1 p2
                            dist  = vnorm d
                            rsum  = radMap M.! c1 + radMap M.! c2
                        in if dist < rsum
                              then
                                let n      = vscale (1/dist) d
                                    r1     = vscale (-radMap M.! c1) n
                                    r2     = vscale  ( radMap M.! c2) n
                                    vRel1  = vadd (vel0 M.! c1)
                                                  (cross (ang0 M.! c1) r1)
                                    vRel2  = vadd (vel0 M.! c2)
                                                  (cross (ang0 M.! c2) r2)
                                    vRel   = vsub vRel1 vRel2
                                    vN     = vdot vRel n
                                in if vN < 0
                                      then
                                        -- normal impulse
                                        let invM1  = 1/(massMap M.! c1)
                                            invM2  = 1/(massMap M.! c2)
                                            term2  = vdot n (applyMat (invIMap M.! c1)
                                                              (cross r1 n))
                                            term3  = vdot n (applyMat (invIMap M.! c2)
                                                              (cross r2 n))
                                            invMn  = invM1 + invM2 + term2 + term3
                                            jn     = -(1 + e) * vN / invMn
                                            impN   = vscale jn n

                                            -- tangential friction
                                            vt     = vsub vRel (vscale vN n)
                                            vtMag  = vnorm vt
                                            tDir   = if vtMag == 0
                                                     then (0,0,0)
                                                     else vscale (1/vtMag) vt
                                            jtMax  = μ * abs jn
                                            invMt1 = invM1 + vdot (cross r1 tDir)
                                                                 (applyMat (invIMap M.! c1)
                                                                           (cross r1 tDir))
                                            invMt2 = invM2 + vdot (cross r2 tDir)
                                                                 (applyMat (invIMap M.! c2)
                                                                           (cross r2 tDir))
                                            invMt  = invMt1 + invMt2
                                            mt     = if invMt /= 0 then 1 / invMt else 0
                                            jt     = min (mt * vtMag) jtMax
                                            impT1  = vscale (-jt) tDir
                                            impT2  = vscale   jt  tDir

                                            -- assemble impulses
                                            d1     = vadd impN impT1
                                            d2     = vadd (vscale (-1) impN) impT2
                                            dv1    = vscale invM1 d1
                                            dv2    = vscale invM2 d2
                                            dw1    = applyMat (invIMap M.! c1)
                                                              (cross r1 d1)
                                            dw2    = applyMat (invIMap M.! c2)
                                                              (cross r2 d2)
                                            dvAcc' = M.insertWith vadd c1 dv1
                                                    $ M.insertWith vadd c2 dv2 dvAcc
                                            dwAcc' = M.insertWith vadd c1 dw1
                                                    $ M.insertWith vadd c2 dw2 dwAcc
                                        in (dvAcc', dwAcc')
                                      else (dvAcc, dwAcc)
                              else (dvAcc, dwAcc)
                     ) (dv0, dw0) [ (c1,c2) | (c1:c2:_) <- tails bodies ]

            -- iterate impulses
            (dvF, dwF) = iterate onePass (dv0, dw0) !! it

            vel' = M.mapWithKey (\c v -> vadd v (dvF  M.! c)) vel0
            ang' = M.mapWithKey (\c w -> vadd w (dwF  M.! c)) ang0

            -- positional correction
            β    = 0.2
            slop = 0.01
            corr = foldl' (\acc (c1,c2) ->
                      let p1       = pos0 M.! c1
                          p2       = pos0 M.! c2
                          d        = vsub p1 p2
                          dist     = vnorm d
                          pen      = radMap M.! c1 + radMap M.! c2 - dist
                          depth    = max 0 (pen - slop)
                      in if depth > 0
                            then
                              let invM1    = 1/(massMap M.! c1)
                                  invM2    = 1/(massMap M.! c2)
                                  sumInv   = invM1 + invM2
                                  corrBase = β * depth
                                  n        = vscale (1 / max 1 dist) d
                                  c1off    = vscale (corrBase * (invM1 / sumInv)) n
                                  c2off    = vscale (-corrBase * (invM2 / sumInv)) n
                              in M.insertWith vadd c1 c1off
                               $ M.insertWith vadd c2 c2off acc
                            else acc
                     ) M.empty [ (c1,c2) | (c1:c2:_) <- tails bodies ]

            pos' = M.mapWithKey
                     (\c p -> vadd p (M.findWithDefault (0,0,0) c corr))
                     pos0

        in st0 { rsPos    = pos'
               , rsVel    = vel'
               , rsAngVel = ang' }

  in RR { domainR = dom, stepR = step }
