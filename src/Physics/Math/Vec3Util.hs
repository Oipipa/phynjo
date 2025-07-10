{-# LANGUAGE Safe #-}


module Physics.Math.Vec3Util
  ( Vec3 
  , vzero
  , vadd, (<+>)
  , vsub, (<->)
  , vscale
  , vdot
  , vnorm2
  , vnorm
  , vhat
  , approxEqVec
  ) where

import           Physics.Integrators.LeapfrogNR (Vec3)

vzero :: Vec3
vzero = (0,0,0)

vadd, vsub :: Vec3 -> Vec3 -> Vec3
vadd (ax,ay,az) (bx,by,bz) = (ax+bx, ay+by, az+bz)
vsub (ax,ay,az) (bx,by,bz) = (ax-bx, ay-by, az-bz)

infixl 6 <+>, <->
(<+>) :: Vec3 -> Vec3 -> Vec3
(<+>) = vadd
(<->) :: Vec3 -> Vec3 -> Vec3
(<->) = vsub

vscale :: Double -> Vec3 -> Vec3
vscale k (x,y,z) = (k*x, k*y, k*z)

vdot :: Vec3 -> Vec3 -> Double
vdot (ax,ay,az) (bx,by,bz) = ax*bx + ay*by + az*bz

vnorm2 :: Vec3 -> Double
vnorm2 v = vdot v v

vnorm :: Vec3 -> Double
vnorm = sqrt . vnorm2

vhat :: Vec3 -> Vec3
vhat v =
  let n = vnorm v
  in if n < 1e-12 then vzero else vscale (1/n) v

approxEqVec :: Vec3 -> Vec3 -> Bool
approxEqVec (ax,ay,az) (bx,by,bz) =
  let epsilon = 1e-9
  in  abs (ax - bx) < epsilon
   && abs (ay - by) < epsilon
   && abs (az - bz) < epsilon
