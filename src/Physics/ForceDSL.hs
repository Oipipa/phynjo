{-# LANGUAGE NamedFieldPuns #-}

module Physics.ForceDSL
  ( addForce
  , addForces
  ) where

import Physics.Leapfrog1D      (leapfrog1D)
import Physics.Force           (Force, (<+>))
import System.SystemForces     (System(..))
import UnitLiteral             (getLiteral)
import NumericWorkflow         (NumericWorkflow(..))
import Components              (Component)
import qualified Data.Map.Strict      as M
import Numeric.Units.Dimensional.Prelude
  ( Quantity, DMass, (/~), kilo, gram )

addForce
  :: Double 
  -> Force 
  -> System
  -> NumericWorkflow
addForce dt f System{sMass} =
  let massMapQ :: M.Map Component (Quantity DMass Double)
      massMapQ = getLiteral sMass

      masses1D :: [(Component, Double)]
      masses1D =
        [ (c, q /~ kilo gram)
        | (c, q) <- M.toList massMapQ
        ]

  in leapfrog1D dt masses1D f

-- | Combine multiple forces and do one leapfrog1D step.
addForces
  :: Double
  -> [Force]
  -> System
  -> NumericWorkflow
addForces _   []    _   = error "addForces: need at least one force"
addForces dt (f:fs) sys = addForce dt (foldr (<+>) f fs) sys
