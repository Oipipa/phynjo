{-# LANGUAGE Safe #-}

module Physics.Collision.BoundingVolume
  ( intersectsAABB
  , intersectsSphere
  , aabbUnion
  , aabbFromSphere
  ) where

import Physics.Collision.Types (AABB(..), SphereBB(..))
import Physics.Math.Vec3Util   (Vec3, vsub, vdot, vzero)

intersectsAABB :: AABB -> AABB -> Bool
intersectsAABB (AABB (min1x,min1y,min1z) (max1x,max1y,max1z))
               (AABB (min2x,min2y,min2z) (max2x,max2y,max2z)) =
     max1x >= min2x && max2x >= min1x
  && max1y >= min2y && max2y >= min1y
  && max1z >= min2z && max2z >= min1z

intersectsSphere :: SphereBB -> SphereBB -> Bool
intersectsSphere (SphereBB c1 r1) (SphereBB c2 r2) =
  let d2 = vdot (vsub c1 c2) (vsub c1 c2)
      r  = r1 + r2
  in d2 <= r * r

aabbUnion :: AABB -> AABB -> AABB
aabbUnion (AABB (min1x,min1y,min1z) (max1x,max1y,max1z))
           (AABB (min2x,min2y,min2z) (max2x,max2y,max2z)) =
  AABB (min min1x min2x, min min1y min2y, min min1z min2z)
       (max max1x max2x, max max1y max2y, max max1z max2z)

aabbFromSphere :: SphereBB -> AABB
aabbFromSphere (SphereBB (cx,cy,cz) r) =
  AABB (cx - r, cy - r, cz - r) (cx + r, cy + r, cz + r)
