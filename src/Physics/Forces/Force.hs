{-# LANGUAGE LambdaCase #-}

module Physics.Forces.Force
  ( Force(..)
  , (<+>)
  , scaleF
  , ForceField
  ) where

import Components                (Component)
import Physics.Math.LinearAlgebra (Vec3)
import NState                    (NState)

type ForceField = NState -> Component -> Vec3

data Force
  = Gravity  Double
  | Spring   Component Component Double Double
  | Drag     Double
  | Custom   ForceField
instance Eq Force where
  Gravity g1           == Gravity g2           = g1 == g2
  Spring i1 j1 k1 l1   == Spring i2 j2 k2 l2   =
    i1 == i2 && j1 == j2 && k1 == k2 && l1 == l2
  Drag d1              == Drag d2              = d1 == d2
  Custom _             == Custom _             = True
  _                    == _                    = False

-- Manual Show instance so Hspec can display failures.
instance Show Force where
  show (Gravity g)       = "Gravity "  ++ show g
  show (Spring i j k l)  = "Spring "   ++ show i ++ " " ++ show j
                                      ++ " " ++ show k ++ " " ++ show l
  show (Drag d)          = "Drag "     ++ show d
  show (Custom _)        = "Custom <force>"

(<+>) :: Force -> Force -> Force
f1 <+> f2 = Custom $ \st c ->
  let v1 = applyFF f1 st c
      v2 = applyFF f2 st c
  in addV v1 v2

scaleF :: Double -> Force -> Force
scaleF s = \case
  Gravity g      -> Gravity (s * g)
  Spring i j k l -> Spring i j (s * k) l
  Drag gamma         -> Drag (s * gamma)
  Custom ff      -> Custom $ \st c -> scaleV s (ff st c)

applyFF :: Force -> ForceField
applyFF (Custom ff) = ff
applyFF _           = error "applyFF: only Custom forces can be applied directly"

addV :: Vec3 -> Vec3 -> Vec3
addV (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

scaleV :: Double -> Vec3 -> Vec3
scaleV s (x,y,z) = (s*x, s*y, s*z)
