module Physics.Math.LinearAlgebra
  ( Vec3
  , vadd, vsub, vscale, vdot, vnorm2, vnorm
  , InertiaTensor
  , invertTensor, applyMat, transpose3, cross
  , quatToMatrix, integrateQuat
  ) where

import Control.Monad (guard)
import Data.Maybe (fromMaybe)

epsilon :: Double
epsilon = 1e-20

type Vec3 = (Double, Double, Double)

vadd :: Vec3 -> Vec3 -> Vec3
vadd (a,b,c) (x,y,z) = (a+x, b+y, c+z)

vsub :: Vec3 -> Vec3 -> Vec3
vsub (a,b,c) (x,y,z) = (a-x, b-y, c-z)

vscale :: Double -> Vec3 -> Vec3
vscale k (x,y,z)
  | isNaN k  = error "vscale: NaN scaling factor"
  | otherwise = (k*x, k*y, k*z)

vdot :: Vec3 -> Vec3 -> Double
vdot (a,b,c) (x,y,z) = a*x + b*y + c*z

vnorm2 :: Vec3 -> Double
vnorm2 v = vdot v v

vnorm :: Vec3 -> Double
vnorm v = sqrt (vnorm2 v)

-- Matrix operations
type InertiaTensor = (Vec3, Vec3, Vec3)

safeInvertTensor :: InertiaTensor -> Maybe InertiaTensor
safeInvertTensor ((a,b,c),(d,e,f),(g,h,i)) = do
  let det = a*(e*i - f*h) - b*(d*i - f*g) + c*(d*h - e*g)
  guard (abs det > 0)
  let invDet = 1 / det
      c11 =  (e*i - f*h); c12 = -(d*i - f*g); c13 =  (d*h - e*g)
      c21 = -(b*i - c*h); c22 =  (a*i - c*g); c23 = -(a*h - b*g)
      c31 =  (b*f - c*e); c32 = -(a*f - c*d); c33 =  (a*e - b*d)
  pure ( (c11*invDet, c12*invDet, c13*invDet)
       , (c21*invDet, c22*invDet, c23*invDet)
       , (c31*invDet, c32*invDet, c33*invDet)
       )

invertTensor :: InertiaTensor -> InertiaTensor
invertTensor tensor = 
  fromMaybe (error "invertTensor: singular matrix") $ safeInvertTensor tensor

applyMat :: InertiaTensor -> Vec3 -> Vec3
applyMat ((a,b,c),(d,e,f),(g,h,i)) (x,y,z)
  | any isNaN [x,y,z] = error "applyMat: NaN vector component"
  | otherwise = ( a*x + b*y + c*z
                , d*x + e*y + f*z
                , g*x + h*y + i*z
                )

transpose3 :: InertiaTensor -> InertiaTensor
transpose3 ((a,b,c),(d,e,f),(g,h,i)) = 
  ((a,d,g),(b,e,h),(c,f,i))

cross :: Vec3 -> Vec3 -> Vec3
cross (x1,y1,z1) (x2,y2,z2)
  | any isNaN [x1,y1,z1,x2,y2,z2] = error "cross: NaN vector component"
  | otherwise = ( y1*z2 - z1*y2
                , z1*x2 - x1*z2
                , x1*y2 - y1*x2
                )

-- Quaternion operations with normalization safeguards
quatToMatrix :: (Double, Double, Double, Double) -> InertiaTensor
quatToMatrix q@(w,x,y,z)
  | any isNaN [w,x,y,z] = error "quatToMatrix: NaN quaternion component"
  | norm < epsilon      = ((1,0,0),(0,1,0),(0,0,1))  -- Identity matrix
  | otherwise           = 
      let w' = w / norm
          x' = x / norm
          y' = y / norm
          z' = z / norm
          ww = w'*w'; xx = x'*x'; yy = y'*y'; zz = z'*z'
          wx = w'*x'; wy = w'*y'; wz = w'*z'
          xy = x'*y'; xz = x'*z'; yz = y'*z'
      in ( (ww+xx-yy-zz,   2*(xy - wz),   2*(xz + wy))
         , (2*(xy + wz),   ww-xx+yy-zz,   2*(yz - wx))
         , (2*(xz - wy),   2*(yz + wx),   ww-xx-yy+zz)
         )
  where norm = sqrt (w*w + x*x + y*y + z*z)

integrateQuat :: Double
              -> Vec3
              -> (Double, Double, Double, Double)
              -> (Double, Double, Double, Double)
integrateQuat dt omega q@(qw,qx,qy,qz)
  | dt <= 0          = error "integrateQuat: non-positive timestep"
  | any isNaN (tupleToList omega) = error "integrateQuat: NaN in angular velocity"
  | any isNaN [qw,qx,qy,qz] = error "integrateQuat: NaN in quaternion"
  | otherwise        =
      let (mw,mx,my,mz) =
            ( -qx*omegax - qy*omegay - qz*omegaz
            ,   qw*omegax + qy*omegaz - qz*omegay
            ,   qw*omegay - qx*omegaz + qz*omegax
            ,   qw*omegaz + qx*omegay - qy*omegax
            )
          half = 0.5 * dt
          qw'  = qw + half * mw
          qx'  = qx + half * mx
          qy'  = qy + half * my
          qz'  = qz + half * mz
          norm = sqrt (qw'*qw' + qx'*qx' + qy'*qy' + qz'*qz')
      in if norm < epsilon
         then (1, 0, 0, 0)  -- Identity quaternion fallback
         else (qw'/norm, qx'/norm, qy'/norm, qz'/norm)
  where
    (omegax, omegay, omegaz) = omega
    tupleToList (a,b,c) = [a,b,c]
