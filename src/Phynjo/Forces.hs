{-# LANGUAGE OverloadedStrings #-}
module Phynjo.Forces
  ( -- Types
    Force(..), Force3D(..), ForceField

    -- Combinators
  , (<+>), scaleF, sumForces3D

    -- 3D primitives
  , runForce3D, gravity3D, spring3D, drag3D

    -- Aerodynamic extras
  , dragQuad3D, magnus3D, dragTorque3D

    -- DSL
  , addForce, addForces

    -- Numeric rules
  , forceNR, gravNR
  ) where

-- Extras (avoid exporting its (<+>) to prevent clashes)
import Physics.Forces.Extra
  ( dragQuad3D
  , magnus3D
  , dragTorque3D
  , sumForces3D
  )

-- Core force API (this is the (<+>) we expose)
import Physics.Forces.Force
  ( Force(..)
  , (<+>)
  , scaleF
  , ForceField
  )

-- 3D forces
import Physics.Forces.Force3D
  ( Force3D(..)
  , runForce3D
  , gravity3D
  , spring3D
  , drag3D
  )

-- DSL helpers
import Physics.Forces.ForceDSL
  ( addForce
  , addForces
  )

-- Numeric rules
import Physics.Forces.ForceNR   ( forceNR )
import Physics.Forces.GravNR    ( gravNR  )
