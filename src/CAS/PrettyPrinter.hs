module CAS.PrettyPrinter (pretty) where

import CAS.AST
import Data.Ratio (numerator, denominator)

pretty :: Expr -> String
pretty = pp 0
  where
    pp :: Int -> Expr -> String
    pp ctx e = case e of
      Var v       -> v
      Const r     -> showR r
      Add a b     -> bracket 1 $ pp 1 a ++ "+" ++ pp 1 b
      Sub a b     -> bracket 1 $ pp 1 a ++ "-" ++ pp 2 b
      Mul a b     -> bracket 2 $ pp 2 a ++ "*" ++ pp 2 b
      Div a b     -> bracket 2 $ pp 2 a ++ "/" ++ pp 3 b
      Pow a b     -> bracket 3 $ pp 4 a ++ "^" ++ pp 3 b
      Neg a       -> bracket 4 $ "-" ++ pp 4 a
      Sin a       -> bracket 4 $ "sin(" ++ pp 0 a ++ ")"
      Cos a       -> bracket 4 $ "cos(" ++ pp 0 a ++ ")"
      Tan a       -> bracket 4 $ "tan(" ++ pp 0 a ++ ")"
      Exp a       -> bracket 4 $ "exp(" ++ pp 0 a ++ ")"
      Log a       -> bracket 4 $ "log(" ++ pp 0 a ++ ")"
      -- catch‐all for future constructors
      _           -> error "PrettyPrinter: unsupported Expr"
      where
        bracket :: Int -> String -> String
        bracket p s = if p < ctx then "(" ++ s ++ ")" else s

        showR :: Rational -> String
        showR r
          | denominator r == 1 = show (numerator r)
          | otherwise          = show (numerator r) ++ "/" ++ show (denominator r)
