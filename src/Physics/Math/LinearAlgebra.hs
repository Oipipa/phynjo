module Physics.Math.LinearAlgebra
  ( Vec3
  , vadd, vsub, vscale, vdot, vnorm2
  , InertiaTensor
  , invertTensor, applyMat, transpose3, cross
  , quatToMatrix, integrateQuat
  ) where

-- 3D vector type and operations
type Vec3 = (Double, Double, Double)

vadd :: Vec3 -> Vec3 -> Vec3
vadd (a,b,c) (x,y,z) = (a+x, b+y, c+z)

vsub :: Vec3 -> Vec3 -> Vec3
vsub (a,b,c) (x,y,z) = (a-x, b-y, c-z)

vscale :: Double -> Vec3 -> Vec3
vscale k (x,y,z) = (k*x, k*y, k*z)

vdot :: Vec3 -> Vec3 -> Double
vdot (a,b,c) (x,y,z) = a*x + b*y + c*z

vnorm2 :: Vec3 -> Double
vnorm2 v = vdot v v

-- Matrix operations
type InertiaTensor = (Vec3, Vec3, Vec3)

invertTensor :: InertiaTensor -> InertiaTensor
invertTensor ((a,b,c),(d,e,f),(g,h,i)) =
  let det    = a*(e*i - f*h) - b*(d*i - f*g) + c*(d*h - e*g)
      invDet = 1 / det
      -- Cofactors (transposed by placement)
      c11 =  (e*i - f*h); c12 = -(d*i - f*g); c13 =  (d*h - e*g)
      c21 = -(b*i - c*h); c22 =  (a*i - c*g); c23 = -(a*h - b*g)
      c31 =  (b*f - c*e); c32 = -(a*f - c*d); c33 =  (a*e - b*d)
  in ( (c11*invDet, c12*invDet, c13*invDet)
     , (c21*invDet, c22*invDet, c23*invDet)
     , (c31*invDet, c32*invDet, c33*invDet)
     )

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

-- Quaternion operations
quatToMatrix :: (Double, Double, Double, Double) -> InertiaTensor
quatToMatrix (w,x,y,z) =
  let ww = w*w; xx = x*x; yy = y*y; zz = z*z
      wx = w*x; wy = w*y; wz = w*z
      xy = x*y; xz = x*z; yz = y*z
  in ( (ww+xx-yy-zz,   2*(xy - wz),   2*(xz + wy))
     , (2*(xy + wz),   ww-xx+yy-zz,   2*(yz - wx))
     , (2*(xz - wy),   2*(yz + wx),   ww-xx-yy+zz)
     )

integrateQuat :: Double
              -> Vec3
              -> (Double, Double, Double, Double)
              -> (Double, Double, Double, Double)
integrateQuat dt (omegax,omegay,omegaz) (qw,qx,qy,qz) =
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
  in (qw'/norm, qx'/norm, qy'/norm, qz'/norm)