{-# LANGUAGE RecordWildCards #-}

module Physics.Contact
  ( contactGround
  , contactSpheres
  ) where

import Components           ( Component )
import Physics.Rigid3DNR    ( RRune(..) )
import Physics.RigidState   ( RigidState(..) )
import Physics.LeapfrogNR   ( Vec3, vadd, vsub, vscale, vdot )
import qualified Data.Map.Strict as M
import           Data.Set        (Set)
import qualified Data.Set        as S

----------------------------------------------------------------------

-- | Bounce off the horizontal plane y=0 with restitution e ∈ [0,1].
--   Bodies in `bodies` whose y<0 and vy<0 will have vy ← -e·vy.
contactGround
  :: Double        -- ^ restitution coefficient e
  -> [Component]   -- ^ bodies to check
  -> RRune
contactGround e bodies =
  let dom = S.fromList bodies
      step _dt st =
        let pos0 = rsPos st
            vel0 = rsVel st
            vel' = M.mapWithKey
                     (\c (vx,vy,vz) ->
                        if c `S.member` dom
                           && let (_,y,_) = pos0 M.! c
                              in y < 0 && vy < 0
                        then (vx, -e * vy, vz)
                        else (vx,vy,vz)
                     )
                     vel0
        in st { rsVel = vel' }
  in RR dom step

contactSpheres
  :: Double                     -- ^ restitution e
  -> [(Component,Double,Double)]-- ^ (body, radius, mass)
  -> RRune
contactSpheres e specs =
  let comps   = [ c | (c,_,_) <- specs ]
      dom     = S.fromList comps
      radMap  = M.fromList [ (c,r) | (c,r,_) <- specs ]
      massMap = M.fromList [ (c,m) | (c,_,m) <- specs ]

      step _dt st =
        let pos0 = rsPos st
            vel0 = rsVel st

            -- accumulate velocity deltas for each body
            dvMap = foldr collidePair M.empty
                      [ (c1,c2)
                      | (c1,_,_) <- specs
                      , (c2,_,_) <- specs
                      , c1 < c2
                      ]

            collidePair (c1,c2) acc =
              let p1   = pos0 M.! c1
                  p2   = pos0 M.! c2
                  d    = vsub p1 p2
                  dist = sqrt (vdot d d)
                  rsum = radMap M.! c1 + radMap M.! c2
              in if dist < rsum
                   then
                     -- unit normal from 2→1
                     let n    = vscale (1/dist) d
                         v1   = vel0 M.! c1
                         v2   = vel0 M.! c2
                         vrel = vdot (vsub v1 v2) n
                     in if vrel < 0
                          then
                            let m1  = massMap M.! c1
                                m2  = massMap M.! c2
                                j   = -(1+e) * vrel / (1/m1 + 1/m2)
                                dv1 = vscale ( j / m1) n
                                dv2 = vscale (-j / m2) n
                                acc' = M.insertWith vadd c1 dv1
                                     $ M.insertWith vadd c2 dv2 acc
                            in acc'
                          else acc
                   else acc

            -- apply the velocity deltas
            vel' = M.mapWithKey
                     (\c v0 ->
                        let dv = M.findWithDefault (0,0,0) c dvMap
                        in vadd v0 dv
                     )
                     vel0

        in st { rsVel = vel' }

  in RR dom step
