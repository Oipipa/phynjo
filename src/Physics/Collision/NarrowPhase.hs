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

data Shape
  = ShSphere Double 
  | ShPlane 
  deriving (Eq, Show)

type InertiaTensor = (Vec3, Vec3, Vec3)

narrowPhase
  :: (Double -> Double) 
  -> (Double -> Double) 
  -> Int 
  -> [(Component, Shape)] 
  -> Set (Component, Component) 
  -> RRune
narrowPhase eFun muFun it shapes pairs =
  let
    comps = concatMap (\(a,b) -> [a,b]) (S.toList pairs)
    uniq  = S.toList (S.fromList comps)
    specs =
      [ (c, r, 1.0, identityTensor)
      | c <- uniq
      , Just (ShSphere r) <- [lookup c shapes]
      ]
  in contactSpheresF eFun muFun it specs

identityTensor :: InertiaTensor
identityTensor = ((1,0,0),(0,1,0),(0,0,1))
