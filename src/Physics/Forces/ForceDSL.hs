{-# LANGUAGE NamedFieldPuns #-}

module Physics.Forces.ForceDSL
  ( addForce
  , addForces
  ) where

import qualified Data.Map.Strict as M

import Physics.Integrators.Leapfrog1D        (leapfrog1D)
import Physics.Forces.Force                  (Force(..))
import System.SystemForces                   (System(..))
import UnitLiteral                           (getLiteral)
import NumericWorkflow                       (NumericWorkflow(..))
import Components                            (Component)
import Numeric.Units.Dimensional.Prelude     (Quantity, DMass, (/~), kilo, gram)

import NState                                (NState, lookupPos, lookupMom)

addForce :: Double -> Force -> System -> NumericWorkflow
addForce dt f System{sMass} =
  let massMapQ :: M.Map Component (Quantity DMass Double)
      massMapQ = getLiteral sMass
      masses1D :: [(Component, Double)]
      masses1D = [ (c, q /~ kilo gram) | (c, q) <- M.toList massMapQ ]
  in leapfrog1D dt masses1D f

addForces :: Double -> [Force] -> System -> NumericWorkflow
addForces _   []    _   = error "addForces: need at least one force"
addForces dt  fs    sys@System{sMass} =
  let massMapQ :: M.Map Component (Quantity DMass Double)
      massMapQ = getLiteral sMass
      masses1D :: [(Component, Double)]
      masses1D = [ (c, q /~ kilo gram) | (c, q) <- M.toList massMapQ ]
      mMap     = M.fromList masses1D
      run1D :: Force -> NState -> Component -> Double
      run1D (Custom ff) st c        = let (fx,_,_) = ff st c in fx
      run1D (Drag gamma) st c       = let p = lookupMom c st
                                          m = mMap M.! c
                                      in (-gamma) * (p / m)
      run1D (Spring i j k rest) st c =
        let xi   = lookupPos i st
            xj   = lookupPos j st
            dx   = xj - xi
            disp = dx - signum dx * rest
        in if c == i then  k * disp
           else if c == j then (-k) * disp
           else 0
      run1D (Gravity g) st c =
        let mi = mMap M.! c
            qi = lookupPos c st
            ids = M.keys mMap
            eps = 1e-12
        in sum
             [ let mj = mMap M.! j
                   qj = lookupPos j st
                   d  = qj - qi
                   r2 = let a = abs d in if a < eps then eps*eps else a*a
               in g * mi * mj * signum d / r2
             | j <- ids, j /= c
             ]
      ff st c =
        let fx = sum [ run1D f st c | f <- fs ]
        in  (fx, 0, 0)
  in addForce dt (Custom ff) sys
