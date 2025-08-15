{-# LANGUAGE RecordWildCards #-}

module Physics.Forces.Force3D
  ( Force3D(..)
  , runForce3D
  , gravity3D
  , spring3D
  , drag3D
  ) where

import Components                               (Component)
import Physics.RigidBodyUtilities.RigidState    (RigidState, lookupPosR, lookupVelR)
import Physics.Math.LinearAlgebra               (Vec3, vsub, vnorm2, vscale)
import qualified Data.Map.Strict as M

newtype Force3D = Force3D
  { runForce3D :: RigidState -> Component -> (Vec3, Vec3) }

gravity3D :: Double -> M.Map Component Double -> Force3D
gravity3D g massMap = Force3D $ \_ c ->
  let m = massMap M.! c
  in ((0, - m * g, 0), (0,0,0))

spring3D :: Component -> Component -> Double -> Double -> Force3D
spring3D i j k rest = Force3D $ \st c ->
  let xi    = lookupPosR i st
      xj    = lookupPosR j st
      dx    = vsub xj xi
      d2    = vnorm2 dx
      d     = sqrt d2
      dir   = if d == 0 then (0,0,0) else vscale (1/d) dx
      mag   = k * (d - rest)
      fi    = vscale  mag dir
      fj    = vscale (-mag) dir
      f     | c == i    = fi
            | c == j    = fj
            | otherwise = (0,0,0)
  in (f, (0,0,0))

drag3D :: Double -> Force3D
drag3D gamma = Force3D $ \st c ->
  let v = lookupVelR c st
      f = vscale (-gamma) v
  in (f, (0,0,0))
