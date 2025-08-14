{-# LANGUAGE OverloadedStrings #-}
module Phynjo.SymbolicPhysics
  ( -- Calculus
    Expr(..) 
  ,  differentiate
  , integrate
  , integrateExp, integrateLog, integratePoly, integrateRat, integrateTrig
  , isExp, isLog, isPolynomial, isRational, isTrig
  , simplify, simplifyAdd, simplifySub, simplifyFix
    -- Lagrangian mechanics
  , Coord(..), LagM
  , defineCoord, timeDeriv, tDerivative
  , buildLagrangian, eulerLagrange
  , qlag, qlagd, qlagdd, pretty
  , (.+.), (.-.), (.*.), (./.), (.^.)
  , neg
  ) where 

import CAS.Differentiate (differentiate)
import CAS.Simplify
  ( simplify
  , simplifyAdd
  , simplifySub
  , simplifyFix
  )
import CAS.PrettyPrinter (pretty)
-- main integrator
import CAS.Integrate.Integrate (integrate)

-- specialized integrators and predicates, kept qualified to avoid name clashes
import qualified CAS.Integrate.Exponential   as Ex
import qualified CAS.Integrate.Logarithmic   as Log
import qualified CAS.Integrate.Polynomial    as Poly
import qualified CAS.Integrate.Rational      as Rat
import qualified CAS.Integrate.Trigonometric as Trig

import Physics.Lagrangian
  ( Coord(..)
  , LagM
  , defineCoord
  , timeDeriv
  , buildLagrangian
  , eulerLagrange
  , tDerivative
  , qlag, qlagd, qlagdd
  )

import CAS.AST ( Expr(..)
  , (.+.), (.-.), (.*.), (./.), (.^.)
  , neg
  )
-- re-exports for specialized integration routines
integrateExp = Ex.integrate
integrateLog = Log.integrate
integratePoly = Poly.integrate
integrateRat = Rat.integrate
integrateTrig = Trig.integrate

-- re-exports for recognizers
isExp        = Ex.isExp
isLog        = Log.isLog
isPolynomial = Poly.isPolynomial
isRational   = Rat.isRational
isTrig       = Trig.isTrig
