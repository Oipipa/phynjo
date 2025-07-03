module CAS.Differentiate (differentiate) where

import           CAS.AST
import           CAS.Simplify       (simplify, simplifyAdd, simplifySub)
import           Data.Ratio         (numerator, denominator, (%))

differentiate :: String -> Expr -> Expr
differentiate v expr = diff expr
  where
    -- collect all factors of a product into a list
    flattenMul :: Expr -> [Expr]
    flattenMul (Mul x y) = flattenMul x ++ flattenMul y
    flattenMul e         = [e]

    -- remove any “1” factors but leave zeros intact
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
      Add
        (stripOneFlatten (Mul (diff a) b))
        (stripOneFlatten (Mul a (diff b)))

    diff (Div a b) =
      Div
        (Sub (Mul (diff a) b) (Mul a (diff b)))
        (Pow b (Const 2))

    diff (Pow a (Const n)) =
      let innerPow
            | denominator n /= 1      = Pow a (Const ((numerator n - 1) % denominator n))
            | numerator n == 1        = a
            | numerator n - 1 == 1    = a
            | otherwise               = Pow a (Const ((numerator n - 1) % 1))
      in Mul (Mul (Const n) innerPow) (Const 1)

    diff (Pow a b) =
      let f  = Pow a b
          da = diff a
          db = diff b
      in if da == Const 0
           then Mul f (stripOneFlatten (Mul db (Log a)))
           else Mul f (Add (Mul db (Log a)) (Div (Mul b da) a))

    diff (Neg a)   = Neg (diff a)
    diff (Sin a)   = let da = stripOneFlatten (diff a)
                     in if da == Const 1 then Cos a else Mul da (Cos a)
    diff (Cos a)   = let da = stripOneFlatten (diff a)
                     in Neg (Mul da (Sin a))
    diff (Tan a)   = let da = stripOneFlatten (diff a)
                     in if da == Const 1
                          then Div (Const 1) (Pow (Cos a) (Const 2))
                          else Mul da (Div (Const 1) (Pow (Cos a) (Const 2)))
    diff (Exp a)   = let da = simplify (diff a)
                     in Mul da (Exp a)
    diff (Log a)   = let da = simplify (diff a)
                     in Div da a

    diff x         = x
