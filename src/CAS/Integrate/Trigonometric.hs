-- src/CAS/Integrate/Trigonometric.hs
module CAS.Integrate.Trigonometric (isTrig, integrate) where

import CAS.AST
import Data.Ratio ((%))

isTrig :: Expr -> Bool
isTrig (Sin _) = True
isTrig (Cos _) = True
isTrig (Tan _) = True
isTrig _       = False

integrate :: Expr -> Expr
integrate expr = case expr of
  Sin arg ->
    let k = coeff arg
    in if k == 1
         then Neg (Cos arg)
         else Div (Neg (Cos arg)) (Const k)

  Cos arg ->
    let k = coeff arg
    in if k == 1
         then Sin arg
         else Div (Sin arg) (Const k)

  Tan arg ->
    let k = coeff arg
    in if k == 1
         then Neg (Log (Cos arg))
         else Div (Neg (Log (Cos arg))) (Const k)

  _ -> expr

  where
    -- extract coefficient k if arg = k * x, else k = 1
    coeff :: Expr -> Rational
    coeff (Mul (Const c) (Var "x")) = c
    coeff (Mul (Var "x") (Const c)) = c
    coeff (Var "x")                 = 1
    coeff _                         = 1
