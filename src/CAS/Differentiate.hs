module CAS.Differentiate (differentiate) where

import           CAS.AST
import           CAS.Simplify       (simplify, simplifyAdd, simplifySub)
import           Data.Ratio         ((%)) 

differentiate :: String -> Expr -> Expr
differentiate v expr = diff expr
  where
    flattenMul :: Expr -> [Expr]
    flattenMul (Mul x y) = flattenMul x ++ flattenMul y
    flattenMul e         = [e]

    stripOneFlatten :: Expr -> Expr
    stripOneFlatten e =
      let fs  = flattenMul e
          fs' = filter (/= Const 1) fs
      in case fs' of
           []  -> Const 1
           [f] -> f
           gs  -> foldl1 Mul gs

    diff :: Expr -> Expr
    diff (Var x)
      | x == v    = Const 1
      | otherwise = Const 0

    diff (Const _) = Const 0

    diff (Add a b) = simplifyAdd (diff a) (diff b)
    diff (Sub a b) = simplifySub (diff a) (diff b)

    diff (Mul a b) =
      let da = diff a
          db = diff b
      in simplify $ Add
           (stripOneFlatten (Mul da b))
           (stripOneFlatten (Mul a db))

    diff (Div a b) =
      simplify $ Div
        (Sub (Mul (diff a) b) (Mul a (diff b)))
        (Pow b (Const 2))

    diff (Pow a (Const n)) =
      let da = diff a
      in simplify $ Mul (Mul (Const n) (Pow a (Const (n - 1)))) da

    diff (Pow a b) =
      let f  = Pow a b
          da = diff a
          db = diff b
      in simplify $ Mul f (Add (Mul db (Log a)) (Div (Mul b da) a))

    diff (Neg a)   = simplify $ Neg (diff a)

    diff (Sin a)   =
      let da = stripOneFlatten (diff a)
      in simplify $ Mul da (Cos a)

    diff (Cos a)   =
      let da = stripOneFlatten (diff a)
      in simplify $ Neg (Mul da (Sin a))

    diff (Tan a)   =
      let da = stripOneFlatten (diff a)
          sec2 = Div (Const 1) (Pow (Cos a) (Const 2))
      in simplify $ Mul da sec2

    diff (Exp a)   = simplify $ Mul (diff a) (Exp a)

    diff (Log a)   = simplify $ Div (diff a) a
