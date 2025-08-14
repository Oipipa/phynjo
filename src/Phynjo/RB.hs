{-# LANGUAGE OverloadedStrings #-}
module Phynjo.RB
  ( -- Core types
    RigidBody(..), InertiaTensor, Quaternion
  , RigidState(..), RRune(..)
    -- Constructors
  , mkRigidBody
  , emptyRigid, insertRigid
  , inertiaId
    -- Lookups
  , lookupPosR, lookupOriR, lookupVelR, lookupAngVelR

    -- Motion updates
  , applyRRuneWorld, driftTrans, driftRot, kickForce3D
  ) where

import Physics.RigidBodyUtilities.Rigid3DNR
  ( RRune(..), applyRRuneWorld, driftTrans, driftRot, kickForce3D )

import Physics.RigidBodyUtilities.RigidBody
  ( RigidBody(..), InertiaTensor, Quaternion, mkRigidBody )

import Physics.RigidBodyUtilities.RigidState
  ( RigidState(..)
  , emptyRigid
  , insertRigid
  , lookupPosR
  , lookupOriR
  , lookupVelR
  , lookupAngVelR
  )

inertiaId :: ((Double,Double,Double),
              (Double,Double,Double),
              (Double,Double,Double))
inertiaId = ((1,0,0),(0,1,0),(0,0,1))