-- src/Physics/ForceDSL.hs
{-# LANGUAGE NamedFieldPuns #-}

module Physics.ForceDSL
  ( addForce
  , addForces
  ) where

import Physics.Force       (Force)
import Physics.ForceNR     (forceNR)
import Physics.DriftNR     (driftNR)
import NSpell               (NSpell(..))
import Components           (Component)
import System.SystemForces  (System(..))
import UnitLiteral          (getLiteral)
import Numeric.Units.Dimensional.Prelude
  ( Quantity, DMass, (/~), kilo, gram )

import qualified Data.Map.Strict   as M

-- | Kick then drift for a single force.
addForce
  :: Double      -- ^ timestep Δt
  -> Force       -- ^ force to apply
  -> System      -- ^ extended system
  -> NSpell
addForce dt f System{sMass} =
  let -- Extract unit-checked mass map
      massMapQ :: M.Map Component (Quantity DMass Double)
      massMapQ = getLiteral sMass

      -- Convert to raw Doubles (kg)
      masses1D :: [(Component, Double)]
      masses1D =
        [ (c, q /~ (kilo gram))
        | (c, q) <- M.toList massMapQ
        ]

      -- Build kick (force) and drift runes
      kick  = NRun (forceNR f masses1D)
      drift = NRun (driftNR masses1D)
  in  NSeq kick drift

-- | Sequence several forces in the order given.
addForces
  :: Double        -- ^ timestep Δt
  -> [Force]       -- ^ list of forces
  -> System        -- ^ extended system
  -> NSpell
addForces dt forces sys =
  case forces of
    []     -> error "addForces: need at least one force"
    (f:fs) ->
      foldl (\acc f' -> NSeq acc (addForce dt f' sys))
            (addForce dt f sys)
            fs
