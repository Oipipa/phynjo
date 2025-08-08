module CAS.PrettyPrinter (pretty) where

import CAS.AST
import Data.Ratio (numerator, denominator)

-- Pretty-print an Expr with sane signs and minimal parentheses.
pretty :: Expr -> String
pretty = pp 0
  where
    -- precedence: 1 = +/-, 2 = */, 3 = ^, 4 = atoms/unary
    pp :: Int -> Expr -> String
    pp ctx e = case e of
      Var v       -> v
      Const r     -> showR r

      -- sign-aware addition: a + (-b) => a-b
      Add a b     ->
        let sA = pp 1 a
            (negB, b') = splitNeg b
            sB = pp 2 b'
            s  = sA ++ (if negB then "-" else "+") ++ sB
        in bracket ctx 1 s

      -- sign-aware subtraction: a - (-b) => a+b
      Sub a b     ->
        let sA = pp 1 a
            (negB, b') = splitNeg b
            sB = pp 2 b'
            s  = sA ++ (if negB then "+" else "-") ++ sB
        in bracket ctx 1 s

      -- pull leading minus out of products; print as "-(a*b)" if needed
      Mul a b     ->
        let (na,a') = splitNeg a
            (nb,b') = splitNeg b
            body    = pp 2 a' ++ "*" ++ pp 2 b'
        in if na /= nb
             then bracket ctx 2 ("-(" ++ body ++ ")")
             else bracket ctx 2 body

      -- same for division
      Div a b     ->
        let (na,a') = splitNeg a
            (nb,b') = splitNeg b
            body    = pp 2 a' ++ "/" ++ pp 3 b'
        in if na /= nb
             then bracket ctx 2 ("-(" ++ body ++ ")")
             else bracket ctx 2 body

      Pow a b     -> bracket ctx 3 $ pp 4 a ++ "^" ++ pp 3 b

      -- unary minus: collapse --x => x, --3 => 3
      Neg a       ->
        let (isNeg, a') = splitNeg a
        in if isNeg then pp 4 a' else "-" ++ pp 4 a'

      Sin a       -> "sin(" ++ pp 0 a ++ ")"
      Cos a       -> "cos(" ++ pp 0 a ++ ")"
      Tan a       -> "tan(" ++ pp 0 a ++ ")"
      Exp a       -> "exp(" ++ pp 0 a ++ ")"
      Log a       -> "log(" ++ pp 0 a ++ ")"

    -- pull out a single leading minus as a flag + positive magnitude
    splitNeg :: Expr -> (Bool, Expr)
    splitNeg (Neg x)             = (True, x)
    splitNeg (Const r) | r < 0   = (True, Const (negate r))
    splitNeg x                   = (False, x)

    -- bracket with explicit ctx
    bracket :: Int -> Int -> String -> String
    bracket ctx p s = if p < ctx then "(" ++ s ++ ")" else s

    showR :: Rational -> String
    showR r
      | denominator r == 1 = show (numerator r)
      | otherwise =
          let n = numerator r
              d = denominator r
          in if n < 0 then "-" ++ show (abs n) ++ "/" ++ show d
                      else show n ++ "/" ++ show d
