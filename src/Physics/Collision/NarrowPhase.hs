{-# LANGUAGE Safe #-}

module Physics.Collision.NarrowPhase
  ( Shape(..)
  , narrowPhase
  ) where

import Physics.Contact      (contactSpheresF)
import Physics.RigidBodyUtilities.Rigid3DNR    (RRune)
import Components            (Component)
import Physics.Math.Vec3Util (Vec3)
import Data.Set             (Set)
import qualified Data.Set as S

-- | Simplified shape for narrow-phase
data Shape
  = ShSphere Double  -- ^ radius
  | ShPlane          -- ^ infinite plane at y=0
  deriving (Eq, Show)

type InertiaTensor = (Vec3, Vec3, Vec3)

-- | Build a narrow-phase collision RRune for sphere–sphere only.
narrowPhase
  :: (Double -> Double)                -- ^ restitution e(vn)
  -> (Double -> Double)                -- ^ friction μ(vt)
  -> Int                               -- ^ solver iterations
  -> [(Component, Shape)]              -- ^ list of shapes
  -> Set (Component, Component)        -- ^ candidate pairs
  -> RRune
narrowPhase eFun μFun it shapes pairs =
  let
    comps = concatMap (\(a,b) -> [a,b]) (S.toList pairs)
    uniq  = S.toList (S.fromList comps)
    specs =
      [ (c, r, 1.0, identityTensor)
      | c <- uniq
      , Just (ShSphere r) <- [lookup c shapes]
      ]
  in contactSpheresF eFun μFun it specs

identityTensor :: InertiaTensor
identityTensor = ((1,0,0),(0,1,0),(0,0,1))
