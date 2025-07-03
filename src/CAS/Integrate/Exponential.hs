-- src/CAS/Integrate/Exponential.hs
module CAS.Integrate.Exponential (isExp, integrate) where

import CAS.AST
import Data.Ratio ((%))

isExp :: Expr -> Bool
isExp (Exp _) = True
isExp _       = False

-- ∫ exp(kx) dx = exp(kx)/k     (for k≠1, and = exp(x) when k=1)
integrate :: Expr -> Expr
integrate expr = case expr of
  Exp arg ->
    let k = coeff arg in
    if k == 1
      then Exp arg
      else Div (Exp arg) (Const k)
  _ -> expr

  where
    -- extract coefficient k if arg = k * x, else k = 1
    coeff :: Expr -> Rational
    coeff (Mul (Const c) (Var "x")) = c
    coeff (Mul (Var "x") (Const c)) = c
    coeff (Var "x")                 = 1
    coeff _                         = 1