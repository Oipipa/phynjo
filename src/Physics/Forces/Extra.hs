{-# LANGUAGE RecordWildCards #-}

module Physics.Forces.Extra
  ( 
    dragQuad3D 
  , magnus3D
  , dragTorque3D 
  , (<+>)
  , sumForces3D
  ) where

import           Physics.Forces.Force3D       (Force3D (..))
import           Physics.RigidBodyUtilities.RigidState    (RigidState, lookupVelR, lookupAngVelR)
import           Physics.Math.LinearAlgebra    (Vec3, vscale, vadd, vdot)
import qualified Data.Map.Strict       as M
import           Data.Semigroup        (Semigroup (..))

vnorm2 :: Vec3 -> Double
vnorm2 v = vdot v v

cross :: Vec3 -> Vec3 -> Vec3
cross (x1,y1,z1) (x2,y2,z2) =
  ( y1*z2 - z1*y2
  , z1*x2 - x1*z2
  , x1*y2 - y1*x2
  )

guardScale :: Bool -> Double -> Vec3 -> Vec3
guardScale ok k v = if ok then vscale k v else (0,0,0)


dragQuad3D
  :: Double          -- air/fluid density rho 
  -> Double          -- drag coefficient C_d 
  -> Double          -- reference area A 
  -> Force3D
dragQuad3D rho cd a = Force3D $ \st c ->
  let v            = lookupVelR c st
      speed2       = vnorm2 v
      forceScale   = 0.5 * rho * cd * a * sqrt speed2
      f            = guardScale (speed2 > 0) (-forceScale) v
  in  (f, (0,0,0))


magnus3D
  :: Double          -- ^ fluid density rho
  -> Double          -- ^ lift coefficient C_l (empirical; 0.05–0.3 for balls)
  -> Double          -- ^ reference area A
  -> Double          -- ^ reference radius r (m)
  -> Force3D
magnus3D rho cl a r = Force3D $ \st c ->
  let v         = lookupVelR   c st
      omega         = lookupAngVelR c st
      f         = vscale (0.5 * rho * cl * a * r) (cross omega v)
  in (f, (0,0,0))

dragTorque3D
  :: Double          -- ^ damping constant c_omega (N·m·s)
  -> Force3D
dragTorque3D comega = Force3D $ \st c ->
  let omega   = lookupAngVelR c st
      τ   = vscale (-comega) omega
  in ((0,0,0), τ)

(<+>) :: Force3D -> Force3D -> Force3D
Force3D f <+> Force3D g = Force3D $ \st c ->
  let (fa,ta) = f st c
      (fb,tb) = g st c
  in  (vadd fa fb, vadd ta tb)
infixr 6 <+>

sumForces3D :: [Force3D] -> Force3D
sumForces3D = foldr1 (<+>)

instance Semigroup Force3D where
  (<>) = (<+>)
instance Monoid Force3D where
  mempty  = Force3D (\_ _ -> ((0,0,0),(0,0,0)))
  mappend = (<>)
