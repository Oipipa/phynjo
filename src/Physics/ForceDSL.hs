-- src/Physics/ForceDSL.hs
{-# LANGUAGE NamedFieldPuns #-}

module Physics.ForceDSL
  ( addForce
  , addForces
  ) where

import Physics.Leapfrog1D   (leapfrog1D)
import Physics.Force        (Force, (<+>))
import System.SystemForces  (System(..))
import UnitLiteral          (getLiteral)
import NSpell               (NSpell)
import Components           (Component)
import qualified Data.Map.Strict as M
import Numeric.Units.Dimensional.Prelude
  ( Quantity, DMass, (/~), kilo, gram )

-- | One leapfrog1D step for a single force.
addForce
  :: Double      -- ^ timestep Δt
  -> Force       -- ^ force to apply
  -> System      -- ^ extended system
  -> NSpell
addForce dt f System{sMass} =
  let massMapQ :: M.Map Component (Quantity DMass Double)
      massMapQ = getLiteral sMass

      masses1D :: [(Component, Double)]
      masses1D =
        [ (c, q /~ (kilo gram))
        | (c, q) <- M.toList massMapQ
        ]

  in leapfrog1D dt masses1D f

-- | Combine multiple forces via (<+>) and do one leapfrog1D step.
addForces
  :: Double
  -> [Force]
  -> System
  -> NSpell
addForces _   []   _   = error "addForces: need at least one force"
addForces dt  fs   sys = addForce dt (foldr1 (<+>) fs) sys
