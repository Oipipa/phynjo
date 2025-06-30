-- src/Physics/RigidBody.hs
{-# LANGUAGE RecordWildCards #-}

module Physics.RigidBody
  ( RigidBody(..)
  , InertiaTensor
  , Quaternion
  , mkRigidBody
  ) where

import Components        (Component)
import Physics.LeapfrogNR (Vec3)

-- | A 3×3 inertia tensor, stored as three row‐vectors.
type InertiaTensor = (Vec3, Vec3, Vec3)

-- | A quaternion (w, x, y, z)
type Quaternion = (Double, Double, Double, Double)

-- | A rigid‐body’s parameters and initial state.
data RigidBody = RigidBody
  { rbIdent   :: Component      -- ^ unique id
  , rbMass    :: Double         -- ^ mass in kg
  , rbInertia :: InertiaTensor  -- ^ inertia tensor about COM
  , rbPos0    :: Vec3           -- ^ initial position (m)
  , rbOri0    :: Quaternion     -- ^ initial orientation
  , rbVel0    :: Vec3           -- ^ initial linear velocity (m/s)
  , rbAngVel0 :: Vec3           -- ^ initial angular velocity (rad/s)
  } deriving (Eq, Show)

-- | Smart‐constructor for a rigid body
mkRigidBody
  :: Component
  -> Double          -- ^ mass
  -> InertiaTensor   -- ^ inertia tensor
  -> Vec3            -- ^ position
  -> Quaternion      -- ^ orientation
  -> Vec3            -- ^ linear velocity
  -> Vec3            -- ^ angular velocity
  -> RigidBody
mkRigidBody ident mass inertia pos ori vel angVel =
  RigidBody{ rbIdent   = ident
           , rbMass    = mass
           , rbInertia = inertia
           , rbPos0    = pos
           , rbOri0    = ori
           , rbVel0    = vel
           , rbAngVel0 = angVel
           }
