{-# LANGUAGE RecordWildCards #-}

module Physics.RigidBodyUtilities.Rigid3DNR
  ( RRune(..)
  , applyRRuneWorld
  , driftTrans
  , driftRot
  , kickForce3D
  ) where

import Components            (Component)
import Physics.RigidBodyUtilities.RigidState    (RigidState(..))
import Physics.Forces.Force3D       (Force3D(..))
import Physics.Math.LinearAlgebra
  ( Vec3, vadd, vsub, vscale
  , InertiaTensor, invertTensor, applyMat, transpose3, cross
  , quatToMatrix, integrateQuat
  )

import qualified Data.Map.Strict as M
import           Data.Set        (Set)
import qualified Data.Set        as S

-- | A numeric‐rune over 3D rigid‐body state.
data RRune = RR
  { domainR :: Set Component
  , stepR   :: Double -> RigidState -> RigidState
  }

applyRRuneWorld :: RRune -> Double -> RigidState -> RigidState
applyRRuneWorld (RR _ step) dt st = step dt st

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
                              -- convert world omega to body omega
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
  -> Force3D                      -- ^ field (F,torque) in world coords
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

                                -- body‐space torque & omega
                                torqueW   = snd (field st c)
                                torqueB   = applyMat rt torqueW
                                wB     = applyMat rt w0

                                iB     = ibodyMap  M.! c
                                lB     = applyMat iB wB

                                invIB  = invIMap   M.! c
                                gyro   = cross wB lB
                                wBdot  = applyMat invIB (vsub torqueB gyro)

                                wWdot  = applyMat rmat wBdot
                            in  vadd w0 (vscale dt wWdot)
                          else w0
                     )
                     ang0

        in st { rsVel    = vel'
              , rsAngVel = ang'
              }

  in RR dom step