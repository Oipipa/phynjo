{-# LANGUAGE OverloadedStrings #-}
module Phynjo.Collisions
  ( -- Core types
    Shape(..)
  , BoundingVolume(..)
  , AABB(..), OBB(..), SphereBB(..)
  , ComponentBB
  , BroadPhase(..)
  , CollisionManager
  , SAP, UniformGrid

  , buildManager, updateManager, runBroadPhase

  , initSAP, updateSAP, potentialPairsSAP

  , initGrid, updateGrid, queryNeighbors, allPairsGrid

  , intersectsAABB, intersectsSphere, aabbUnion, aabbFromSphere

  , narrowPhase

  , contactGroundF, contactSpheresF
  ) where 

import Physics.Collision.BoundingVolume
  ( intersectsAABB, intersectsSphere, aabbUnion, aabbFromSphere )

import Physics.Collision.Manager
  ( BroadPhase(..), CollisionManager, buildManager, updateManager, runBroadPhase )

import Physics.Collision.BroadPhase.SweepAndPrune
  ( SAP, initSAP, updateSAP, potentialPairsSAP )

import Physics.Collision.BroadPhase.UniformGrid
  ( UniformGrid, initGrid, updateGrid, queryNeighbors, allPairsGrid )

import Physics.Collision.NarrowPhase
  ( Shape(..), narrowPhase )

import Physics.Collision.Types
  ( ComponentBB, BoundingVolume(..), AABB(..), SphereBB(..), OBB(..) )

import Physics.Contact
  ( contactGroundF, contactSpheresF )
