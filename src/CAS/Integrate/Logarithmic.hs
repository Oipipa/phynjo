module CAS.Integrate.Logarithmic (isLog, integrate) where

import CAS.AST
import Data.Ratio ((%))

isLog :: Expr -> Bool
isLog (Div (Const r) (Var _)) = r == 1
isLog (Log _)                 = True
isLog _                       = False

integrate :: Expr -> Expr
integrate expr = case expr of
  Div (Const r) (Var v)
    | r == 1    -> Log (Var v)
    | otherwise -> expr
  Log u         -> Sub (Mul u (Log u)) u
  _             -> expr