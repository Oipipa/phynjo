-- src/Physics/Force.hs
{-# LANGUAGE LambdaCase #-}

module Physics.Force
  ( Force(..)
  , (<+>)
  , scaleF
  , ForceField
  ) where

import Components                (Component)
import Physics.LeapfrogNR        (Vec3)
import NState                    (NState)

-- | A spatially- and state-dependent force on each body.
--   Given the full state, returns the force vector on that one component.
type ForceField = NState -> Component -> Vec3

-- | First-class forces in our DSL.
data Force
  = Gravity  Double                             -- ^ gravitational constant G
  | Spring   Component Component Double Double  -- ^ bodies i, j, spring-constant k, rest-length ℓ
  | Drag     Double                             -- ^ damping coefficient γ
  | Custom   ForceField                         -- ^ arbitrary user-supplied force field

-- Manual Eq instance so we can compare built-ins in tests.  
-- Two Customs are always considered equal (function equality is opaque).
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
  show (Spring i j k ℓ)  = "Spring "   ++ show i ++ " " ++ show j
                                      ++ " " ++ show k ++ " " ++ show ℓ
  show (Drag d)          = "Drag "     ++ show d
  show (Custom _)        = "Custom <force>"

-- | Pointwise addition of two forces; always returns a Custom-force
--   that sums the two underlying vector fields.
(<+>) :: Force -> Force -> Force
f1 <+> f2 = Custom $ \st c ->
  let v1 = applyFF f1 st c
      v2 = applyFF f2 st c
  in addV v1 v2

-- | Scale a force by a pure scalar.  For built-ins we simply scale
--   their parameters; for Custom we scale the returned vectors.
scaleF :: Double -> Force -> Force
scaleF s = \case
  Gravity g      -> Gravity (s * g)
  Spring i j k ℓ -> Spring i j (s * k) ℓ
  Drag γ         -> Drag (s * γ)
  Custom ff      -> Custom $ \st c -> scaleV s (ff st c)

-- | Helper to turn any Force into its vector field.
--   Only Custom can be directly applied.
applyFF :: Force -> ForceField
applyFF (Custom ff) = ff
applyFF _           = error "applyFF: only Custom forces can be applied directly"

-- | 3D vector addition
addV :: Vec3 -> Vec3 -> Vec3
addV (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

-- | 3D vector scaling
scaleV :: Double -> Vec3 -> Vec3
scaleV s (x,y,z) = (s*x, s*y, s*z)
