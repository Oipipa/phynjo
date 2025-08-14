module CAS.Integrate.Rational (isRational, integrate) where

import CAS.AST
import Data.Ratio

isLinear :: Expr -> Bool
isLinear e = case e of
  Const _            -> True
  Var _              -> True
  Mul (Const _) (Var _) -> True
  Mul (Var _) (Const _) -> True
  Add a b            -> isLinear a && isLinear b
  Sub a b            -> isLinear a && isLinear b
  _                  -> False

linearCoeffs :: Expr -> (Rational, Rational, String)
linearCoeffs e = case e of
  Add a b -> case (a,b) of
    (Mul (Const a1) (Var v), Const b1) -> (a1, b1, v)
    (Const b1, Mul (Const a1) (Var v)) -> (a1, b1, v)
    (Var v, Const b1)                  -> (1, b1, v)
    (Const b1, Var v)                  -> (1, b1, v)
    _                                  -> error "Not linear"
  Mul (Const a1) (Var v)               -> (a1, 0, v)
  Mul (Var v) (Const a1)               -> (a1, 0, v)
  Var v                               -> (1, 0, v)
  Const b1                             -> (0, b1, "x")
  _                                   -> error "Not linear"

isRational :: Expr -> Bool
isRational (Div num den) = isLinear num && isLinear den
isRational _             = False

integrate :: Expr -> Expr
integrate expr@(Div num den)
  | isRational expr =
      let (a,b,v) = linearCoeffs num
          (c,d,_) = linearCoeffs den
          x       = Var v
          u       = Add (Mul (Const c) x) (Const d)
          aOverC  = Const (a/c)
          k       = Const ((b*c - a*d) / (c*c))
      in Add (Mul aOverC x) (Mul k (Log u))
  | otherwise = expr
integrate e = e