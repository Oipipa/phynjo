{-# LANGUAGE Safe #-}

module Physics.Collision.Types
  ( ComponentBB
  , BoundingVolume(..)
  , AABB(..)
  , SphereBB(..)
  , OBB(..)
  ) where

import Components (Component)
import Physics.Math.Vec3Util (Vec3)

type ComponentBB = (Component, BoundingVolume)

data BoundingVolume
  = BB_AABB AABB
  | BB_Sphere SphereBB
  | BB_OBB OBB
  deriving (Eq, Show)

data AABB = AABB
  { aMin :: Vec3
  , aMax :: Vec3
  } deriving (Eq, Show)

data SphereBB = SphereBB
  { sCenter :: Vec3
  , sRadius :: Double
  } deriving (Eq, Show)

data OBB = OBB
  { oCenter    :: Vec3
  , oHalfSizes :: Vec3
  , oRotation  :: (Double, Double, Double, Double)
  } deriving (Eq, Show)
