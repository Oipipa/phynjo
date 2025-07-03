module CAS.Simplify
  ( simplify
  , simplifyAdd
  , simplifySub
  ) where

import           CAS.AST
import           Data.Ratio

simplify :: Expr -> Expr
simplify (Add a b) = simplifyAdd (simplify a) (simplify b)
simplify (Sub a b) = simplifySub (simplify a) (simplify b)
simplify (Mul a b) = simplifyMul (simplify a) (simplify b)
simplify (Div a b) = simplifyDiv (simplify a) (simplify b)
simplify (Pow a b) = simplifyPow (simplify a) (simplify b)
simplify (Neg a)   = simplifyNeg (simplify a)
simplify e         = e

zero, one :: Expr
zero = Const 0
one  = Const 1

isZero, isOne :: Expr -> Bool
isZero (Const r) = r == 0
isZero _         = False
isOne  (Const r) = r == 1
isOne  _         = False

simplifyAdd :: Expr -> Expr -> Expr
simplifyAdd (Const x) (Const y) = Const (x + y)
simplifyAdd x y
  | isZero x  = y
  | isZero y  = x
  | otherwise = Add x y

simplifySub :: Expr -> Expr -> Expr
simplifySub (Const x) (Const y) = Const (x - y)
simplifySub x y
  | isZero y  = x
  | x == y    = zero
  | isZero x  = Neg y
  | otherwise = Sub x y

simplifyMul :: Expr -> Expr -> Expr
simplifyMul (Const x) (Const y) = Const (x * y)
simplifyMul x y
  | isZero x || isZero y = zero
  | isOne x              = y
  | isOne y              = x
  | otherwise            = Mul x y

simplifyDiv :: Expr -> Expr -> Expr
simplifyDiv (Const x) (Const y) = Const (x / y)
simplifyDiv x y
  | isZero x  = zero
  | isOne y   = x
  | otherwise = Div x y

simplifyPow :: Expr -> Expr -> Expr
simplifyPow _ (Const 0)         = one
simplifyPow x (Const 1)         = x
simplifyPow (Const 0) _         = zero
simplifyPow (Const 1) _         = one
simplifyPow (Const b) (Const e)
  | denominator e == 1 =
      let n = numerator b
          d = denominator b
          k = fromInteger (numerator e)
      in Const ((n^k) % (d^k))
simplifyPow x y               = Pow x y

simplifyNeg :: Expr -> Expr
simplifyNeg (Const x) = Const (negate x)
simplifyNeg (Neg x)   = x
simplifyNeg x         = Neg x
