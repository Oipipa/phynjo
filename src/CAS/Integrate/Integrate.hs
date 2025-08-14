module CAS.Integrate.Integrate (integrate) where

import CAS.AST
import qualified CAS.Integrate.Polynomial   as P
import qualified CAS.Integrate.Rational     as R
import qualified CAS.Integrate.Trigonometric as T
import qualified CAS.Integrate.Exponential  as E
import qualified CAS.Integrate.Logarithmic  as L

integrate :: Expr -> Expr
integrate expr
  | P.isPolynomial expr   = P.integrate expr
  | R.isRational expr     = R.integrate expr
  | T.isTrig expr         = T.integrate expr
  | E.isExp expr          = E.integrate expr
  | L.isLog expr          = L.integrate expr
  | otherwise             = expr
