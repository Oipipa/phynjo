{-# LANGUAGE RecordWildCards #-}

module Physics.Force3D
  ( Force3D(..)
  , runForce3D
  , gravity3D
  , spring3D
  , drag3D
  ) where

import Components             (Component)
import Physics.RigidState     (RigidState, lookupPosR, lookupVelR)
import Physics.LeapfrogNR     (Vec3, vsub, vnorm2, vscale)
import qualified Data.Map.Strict as M

-- | A 3D force–torque field on rigid bodies.
newtype Force3D = Force3D
  { runForce3D :: RigidState -> Component -> (Vec3, Vec3) }

-- | Uniform gravity down the –Y axis: F = m·g·(0,–1,0)
gravity3D
  :: Double                -- ^ gravitational acceleration g > 0
  -> M.Map Component Double-- ^ masses (kg) for each body
  -> Force3D
gravity3D g massMap = Force3D $ \_ c ->
  let m = massMap M.! c
  in ((0, - m * g, 0), (0,0,0))

-- | Hooke spring between bodies i and j:
--     F = –k·(‖xⱼ–xᵢ‖ – rest) · (unit vector from i to j)
spring3D
  :: Component  -- ^ body i
  -> Component  -- ^ body j
  -> Double     -- ^ spring constant k
  -> Double     -- ^ rest length
  -> Force3D
spring3D i j k rest = Force3D $ \st c ->
  let xi    = lookupPosR i st
      xj    = lookupPosR j st
      dx    = vsub xj xi
      dist2 = vnorm2 dx
      dist  = sqrt dist2
      dir   = if dist == 0 then (0,0,0) else vscale (1/dist) dx
      mag   = k * (dist - rest)
      f_i   = vscale  mag dir
      f_j   = vscale (-mag) dir
      f     | c == i    = f_i
            | c == j    = f_j
            | otherwise = (0,0,0)
  in (f, (0,0,0))

-- | Linear drag: F = –γ·v, where v is the body’s linear velocity.
drag3D
  :: Double    -- ^ drag coefficient γ
  -> Force3D
drag3D γ = Force3D $ \st c ->
  let v = lookupVelR c st
      f = vscale (-γ) v
  in (f, (0,0,0))
