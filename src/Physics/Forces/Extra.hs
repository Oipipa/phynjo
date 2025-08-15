{-# LANGUAGE RecordWildCards #-}

module Physics.Forces.Extra
  ( dragQuad3D
  , magnus3D
  , dragTorque3D
  , (<+>)
  , sumForces3D
  ) where

import Physics.Forces.Force3D                 (Force3D(..))
import Physics.RigidBodyUtilities.RigidState  (RigidState, lookupVelR, lookupAngVelR)
import Physics.Math.LinearAlgebra             (Vec3, vscale, vadd, vnorm2, cross)
import Data.Semigroup                         (Semigroup(..))

guardScale :: Bool -> Double -> Vec3 -> Vec3
guardScale ok k v = if ok then vscale k v else (0,0,0)

dragQuad3D :: Double -> Double -> Double -> Force3D
dragQuad3D rho cd a = Force3D $ \st c ->
  let v      = lookupVelR c st
      s2     = vnorm2 v
      s      = sqrt s2
      f      = guardScale (s2 > 0) (-0.5 * rho * cd * a * s) v
  in  (f, (0,0,0))

magnus3D :: Double -> Double -> Double -> Double -> Force3D
magnus3D rho cl a r = Force3D $ \st c ->
  let v   = lookupVelR c st
      w   = lookupAngVelR c st
      f   = vscale (0.5 * rho * cl * a * r) (cross w v)
  in (f, (0,0,0))

dragTorque3D :: Double -> Force3D
dragTorque3D comega = Force3D $ \st c ->
  let w = lookupAngVelR c st
      t = vscale (-comega) w
  in ((0,0,0), t)

(<+>) :: Force3D -> Force3D -> Force3D
Force3D f <+> Force3D g = Force3D $ \st c ->
  let (fa,ta) = f st c
      (fb,tb) = g st c
  in  (vadd fa fb, vadd ta tb)
infixr 6 <+>

sumForces3D :: [Force3D] -> Force3D
sumForces3D = mconcat

instance Semigroup Force3D where
  (<>) = (<+>)

instance Monoid Force3D where
  mempty  = Force3D (\_ _ -> ((0,0,0),(0,0,0)))
  mappend = (<>)
