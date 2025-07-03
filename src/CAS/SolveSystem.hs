module CAS.SolveSystem (solveSystem) where

import CAS.AST
import CAS.Simplify     (simplify)

-- | Solve a 2×2 linear system for variables [v1,v2].
--   Equations are given as (lhs, rhs) meaning lhs = rhs.
solveSystem :: [String] -> [(Expr, Expr)] -> [(String, Expr)]
solveSystem [v1,v2] [(lhs1, rhs1), (lhs2, rhs2)] =
  let
      -- move all terms to LHS: eq = lhs - rhs = 0
      eqA = Sub lhs1 rhs1
      eqB = Sub lhs2 rhs2

      -- coefficient of v in an expression
      coeff :: Expr -> String -> Rational
      coeff expr v = case expr of
        Add a b -> coeff a v + coeff b v
        Sub a b -> coeff a v - coeff b v
        Mul (Const c) (Var x) | x == v -> c
        Mul (Var x) (Const c) | x == v -> c
        Var x | x == v               -> 1
        Const _                      -> 0
        _                            -> 0

      -- constant term in expression
      constTerm :: Expr -> Rational
      constTerm expr = case expr of
        Add a b -> constTerm a + constTerm b
        Sub a b -> constTerm a - constTerm b
        Const c -> c
        _       -> 0

      -- extract numeric coeffs
      a  = coeff eqA v1
      b  = coeff eqA v2
      c  = coeff eqB v1
      d  = coeff eqB v2
      c1 = constTerm eqA
      c2 = constTerm eqB

      -- determinant and moved constants
      det = a*d - b*c
      e   = -c1
      f   = -c2

      -- Cramer's rule numerators
      xNum = e*d - b*f
      yNum = a*f - e*c

      solX = simplify $ Const (xNum / det)
      solY = simplify $ Const (yNum / det)
  in [(v1, solX), (v2, solY)]
solveSystem _ _ = error "solveSystem: only 2×2 systems supported"
