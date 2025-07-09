{-# LANGUAGE RecordWildCards #-}

module Physics.RigidBodyUtilities.RigidBody
  ( RigidBody(..)
  , InertiaTensor
  , Quaternion
  , mkRigidBody
  ) where

import Components        (Component)
import Physics.Integrators.LeapfrogNR (Vec3)

type InertiaTensor = (Vec3, Vec3, Vec3)

type Quaternion = (Double, Double, Double, Double)

data RigidBody = RigidBody
  { rbIdent   :: Component 
  , rbMass    :: Double 
  , rbInertia :: InertiaTensor 
  , rbPos0    :: Vec3 
  , rbOri0    :: Quaternion 
  , rbVel0    :: Vec3 
  , rbAngVel0 :: Vec3 
  } deriving (Eq, Show)

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
