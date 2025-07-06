{-# LANGUAGE RecordWildCards #-}

module Physics.Rigid3DNR
  ( RRune(..)
  , applyRRuneWorld
  , driftTrans
  , driftRot
  , kickForce3D
  ) where

import Components            (Component)
import Physics.RigidState    (RigidState(..))
import Physics.Force3D       (Force3D(..))
import Physics.LeapfrogNR    ( Vec3
                             , vadd
                             , vsub
                             , vscale )

import qualified Data.Map.Strict as M
import           Data.Set        (Set)
import qualified Data.Set        as S

-- | A numeric‐rune over 3D rigid‐body state.
data RRune = RR
  { domainR :: Set Component
  , stepR   :: Double -> RigidState -> RigidState
  }

-- | Apply one step of an RRune.
applyRRuneWorld :: RRune -> Double -> RigidState -> RigidState
applyRRuneWorld (RR _ step) dt st = step dt st

-- | Drift translation: rᵢ ← rᵢ + dt·vᵢ for i in domain.
driftTrans :: [Component] -> RRune
driftTrans comps =
  let dom = S.fromList comps
      step dt st =
        let pos0 = rsPos st
            vel0 = rsVel st
            pos' = M.mapWithKey
                     (\c r0 ->
                        if c `S.member` dom
                          then vadd r0 (vscale dt (vel0 M.! c))
                          else r0
                     )
                     pos0
        in st { rsPos = pos' }
  in RR dom step

-- | Drift rotation: qᵢ ← integrateQuat(dt, ωᵢ_body, qᵢ), only for i∈domain.
driftRot :: [Component] -> RRune
driftRot comps =
  let dom = S.fromList comps
      step dt st =
        let ori0   = rsOri st
            velW   = rsAngVel st
            ori'   = M.mapWithKey
                       (\c q0 ->
                          if c `S.member` dom
                            then
                              -- convert world ω to body ω
                              let rot    = quatToMatrix q0
                                  rotT   = transpose3 rot
                                  omegaW = velW M.! c
                                  omegaB = applyMat rotT omegaW
                              in  integrateQuat dt omegaB q0
                            else q0
                       )
                       ori0
        in st { rsOri = ori' }
  in RR dom step

-- | Kick: full Euler‐equation update + linear‐velocity kick.
kickForce3D
  :: [(Component, Double)]        -- ^ mass map (kg)
  -> [(Component, InertiaTensor)] -- ^ body‐space inertia tensors
  -> Force3D                      -- ^ field (F,τ) in world coords
  -> RRune
kickForce3D masses inertias (Force3D field) =
  let dom      = S.fromList (map fst masses)
      massMap  = M.fromList masses
      ibodyMap = M.fromList inertias
      invIMap  = fmap invertTensor ibodyMap

      step dt st =
        let vel0 = rsVel st
            ang0 = rsAngVel st
            ori0 = rsOri st

            -- linear update: v ← v + (dt/m)·F
            vel' = M.mapWithKey
                     (\c v0 ->
                        if c `S.member` dom
                          then let (f,_) = field st c
                                   m      = massMap M.! c
                               in  vadd v0 (vscale (dt / m) f)
                          else v0
                     )
                     vel0

            -- angular update via Euler equations
            ang' = M.mapWithKey
                     (\c w0 ->
                        if c `S.member` dom
                          then
                            let -- world→body
                                q      = ori0 M.! c
                                rmat   = quatToMatrix q
                                rt     = transpose3 rmat

                                -- body‐space torque & ω
                                tauW   = snd (field st c)
                                tauB   = applyMat rt tauW
                                wB     = applyMat rt w0

                                iB     = ibodyMap  M.! c
                                lB     = applyMat iB wB

                                invIB  = invIMap   M.! c
                                gyro   = cross wB lB
                                wBdot  = applyMat invIB (vsub tauB gyro)

                                wWdot  = applyMat rmat wBdot
                            in  vadd w0 (vscale dt wWdot)
                          else w0
                     )
                     ang0

        in st { rsVel    = vel'
              , rsAngVel = ang'
              }

  in RR dom step

----------------------------------------------------------------------  
--  3×3 inertia‐tensor utilities

type InertiaTensor = (Vec3, Vec3, Vec3)

-- | Correct cofactor‐based inversion (adjugate transpose).
invertTensor :: InertiaTensor -> InertiaTensor
invertTensor ((a,b,c),(d,e,f),(g,h,i)) =
  let det    = a*(e*i - f*h)
             - b*(d*i - f*g)
             + c*(d*h - e*g)
      invDet = 1 / det

      -- cofactors (then transposed immediately by placing)
      c11 =  (e*i - f*h); c12 = -(d*i - f*g); c13 =  (d*h - e*g)
      c21 = -(b*i - c*h); c22 =  (a*i - c*g); c23 = -(a*h - b*g)
      c31 =  (b*f - c*e); c32 = -(a*f - c*d); c33 =  (a*e - b*d)

  in ( (c11*invDet, c12*invDet, c13*invDet)
     , (c21*invDet, c22*invDet, c23*invDet)
     , (c31*invDet, c32*invDet, c33*invDet)
     )

-- | Matrix‐vector multiply (row‐triples).
applyMat :: InertiaTensor -> Vec3 -> Vec3
applyMat ((a,b,c),(d,e,f),(g,h,i)) (x,y,z) =
  ( a*x + b*y + c*z
  , d*x + e*y + f*z
  , g*x + h*y + i*z
  )

transpose3 :: InertiaTensor -> InertiaTensor
transpose3 ((a,b,c),(d,e,f),(g,h,i)) =
  ((a,d,g),(b,e,h),(c,f,i))

cross :: Vec3 -> Vec3 -> Vec3
cross (x1,y1,z1) (x2,y2,z2) =
  ( y1*z2 - z1*y2
  , z1*x2 - x1*z2
  , x1*y2 - y1*x2
  )

quatToMatrix :: (Double,Double,Double,Double) -> InertiaTensor
quatToMatrix (w,x,y,z) =
  let ww = w*w; xx = x*x; yy = y*y; zz = z*z
      wx = w*x; wy = w*y; wz = w*z
      xy = x*y; xz = x*z; yz = y*z
  in ( (ww+xx-yy-zz,   2*(xy - wz),   2*(xz + wy))
     , (2*(xy + wz),   ww-xx+yy-zz,   2*(yz - wx))
     , (2*(xz - wy),   2*(yz + wx),   ww-xx-yy+zz)
     )

integrateQuat
  :: Double
  -> Vec3                           -- ^ ω in body coords
  -> (Double,Double,Double,Double) -- ^ (w,x,y,z)
  -> (Double,Double,Double,Double)
integrateQuat dt (ωx,ωy,ωz) (qw,qx,qy,qz) =
  let (mw,mx,my,mz) =
        ( - qx*ωx - qy*ωy - qz*ωz
        ,   qw*ωx + qy*ωz - qz*ωy
        ,   qw*ωy - qx*ωz + qz*ωx
        ,   qw*ωz + qx*ωy - qy*ωx
        )
      half = 0.5 * dt
      qw'  = qw + half * mw
      qx'  = qx + half * mx
      qy'  = qy + half * my
      qz'  = qz + half * mz
      norm = sqrt (qw'*qw' + qx'*qx' + qy'*qy' + qz'*qz')
  in (qw'/norm, qx'/norm, qy'/norm, qz'/norm)
