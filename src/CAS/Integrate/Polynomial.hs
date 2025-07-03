-- src/CAS/Integrate/Polynomial.hs
module CAS.Integrate.Polynomial (isPolynomial, integrate) where

import CAS.AST
import Data.Ratio (numerator, denominator, (%))

-- | True if the expression is a polynomial in x (non‐negative integer exponents)
isPolynomial :: Expr -> Bool
isPolynomial (Const _)       = True
isPolynomial (Var _)         = True
isPolynomial (Add a b)       = isPolynomial a && isPolynomial b
isPolynomial (Sub a b)       = isPolynomial a && isPolynomial b
isPolynomial (Mul a b)       = isPolynomial a && isPolynomial b
isPolynomial (Pow base (Const r)) =
     isPolynomial base
  && denominator r == 1
  && numerator r >= 0
isPolynomial _               = False

-- | Integrate a polynomial (in x) term‐by‐term, with respect to x
integrate :: Expr -> Expr
integrate expr = case expr of
  Const c           -> Mul (Const c) (Var "x")
  Var v
    | v == "x"       -> Div (Pow (Var "x") (Const 2)) (Const 2)
    | otherwise      -> Mul (Var v) (Var "x")
  Add a b           -> Add (integrate a) (integrate b)
  Sub a b           -> Sub (integrate a) (integrate b)
  Mul (Const c) e   -> Mul (Const c) (integrate e)
  Mul e (Const c)   -> Mul (Const c) (integrate e)
  Pow (Var "x") (Const n)
    | denominator n == 1 ->
        let k = numerator n + 1
        in Div (Pow (Var "x") (Const (k % 1))) (Const (k % 1))
  _                  -> expr
