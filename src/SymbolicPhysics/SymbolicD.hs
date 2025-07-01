{-# LANGUAGE LambdaCase #-}

module SymbolicPhysics.SymbolicD
  ( Expr(..)
  , var, constant
  , neg, add, sub, mul, divE, pow
  , sinE, cosE, tanE, expE, logE
  , deriv
  , simplify
  , eval
  , pretty
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Text.Printf     (printf)

-- | Symbolic expression AST
data Expr
  = Var String
  | Const Double
  | Neg  Expr
  | Add  Expr Expr
  | Sub  Expr Expr
  | Mul  Expr Expr
  | Div  Expr Expr
  | Pow  Expr Expr
  | Sin  Expr
  | Cos  Expr
  | Tan  Expr
  | Exp  Expr
  | Log  Expr
  deriving (Eq, Show)

-- | Smart constructors
var      :: String -> Expr
var      = Var

constant :: Double -> Expr
constant = Const

neg  :: Expr -> Expr
neg  = Neg

add  :: Expr -> Expr -> Expr
add  = Add

sub  :: Expr -> Expr -> Expr
sub  = Sub

mul  :: Expr -> Expr -> Expr
mul  = Mul

divE :: Expr -> Expr -> Expr
divE = Div

pow  :: Expr -> Expr -> Expr
pow  = Pow

sinE :: Expr -> Expr
sinE = Sin

cosE :: Expr -> Expr
cosE = Cos

tanE :: Expr -> Expr
tanE = Tan

expE :: Expr -> Expr
expE = Exp

logE :: Expr -> Expr
logE = Log

-- | Symbolic derivative w.r.t. a variable name.
deriv :: String -> Expr -> Expr
deriv x = \case
  Var y
    | x == y    -> Const 1
    | otherwise -> Const 0
  Const _       -> Const 0
  Neg u         -> neg (deriv x u)
  Add u v       -> add (deriv x u) (deriv x v)
  Sub u v       -> sub (deriv x u) (deriv x v)
  Mul u v       -> add (mul (deriv x u) v)
                       (mul u (deriv x v))
  Div u v       -> divE
                     ( sub (mul (deriv x u) v)
                           (mul u (deriv x v)) )
                     ( mul v v )
  Pow u (Const n) ->
                   -- d(u^n)/dx = n·u^(n-1)·u'
                   mul (mul (Const n) (pow u (Const (n-1))))
                       (deriv x u)
  Pow u v       ->
                   -- general power rule: u^v·(v'·ln u + v·u'/u)
                   let u' = deriv x u
                       v' = deriv x v
                   in mul (pow u v)
                          (add (mul v' (logE u))
                               (mul v (divE u' u)))
  Sin u         -> mul (deriv x u) (cosE u)
  Cos u         -> mul (deriv x u) (neg (sinE u))
  Tan u         -> mul (deriv x u) (add (Const 1) (pow (tanE u) (Const 2)))
  Exp u         -> mul (deriv x u) (expE u)
  Log u         -> divE (deriv x u) u

-- | Simplify an expression by constant‐folding and trivial reductions.
simplify :: Expr -> Expr
simplify = \case
  Neg (Const c)      -> Const (-c)
  Neg (Neg u)        -> simplify u
  Neg u              -> Neg (simplify u)

  Add u v -> case (simplify u, simplify v) of
    (Const 0, v')     -> v'
    (u', Const 0)     -> u'
    (Const a, Const b)-> Const (a+b)
    (u', v')          -> Add u' v'

  Sub u v -> case (simplify u, simplify v) of
    (u', Const 0)     -> u'
    (Const a, Const b)-> Const (a-b)
    (u', v')          -> Sub u' v'

  Mul u v -> case (simplify u, simplify v) of
    (Const 0, _     ) -> Const 0
    (_     , Const 0) -> Const 0
    (Const 1, v'    ) -> v'
    (u'    , Const 1) -> u'
    (Const a, Const b)-> Const (a*b)
    (u', v')          -> Mul u' v'

  Div u v -> case (simplify u, simplify v) of
    (_     , Const 1) -> simplify u
    (Const 0, _     ) -> Const 0
    (Const a, Const b)-> Const (a/b)
    (u', v')          -> Div u' v'

  Pow u v -> case (simplify u, simplify v) of
    (Const 0, _     ) -> Const 0
    (_     , Const 0) -> Const 1
    (u', Const 1)     -> u'
    (Const a, Const b)-> Const (a**b)
    (u', v')          -> Pow u' v'

  Sin u   -> Sin (simplify u)
  Cos u   -> Cos (simplify u)
  Tan u   -> Tan (simplify u)
  Exp u   -> Exp (simplify u)
  Log u   -> Log (simplify u)

  v@Var{}  -> v
  c@Const{}-> c

-- | Numerically evaluate an expression given var→Double map (unbound→0).
eval :: Map String Double -> Expr -> Double
eval env = \case
  Var x       -> fromMaybe 0 (M.lookup x env)
  Const c     -> c
  Neg u       -> negate (eval env u)
  Add u v     -> eval env u + eval env v
  Sub u v     -> eval env u - eval env v
  Mul u v     -> eval env u * eval env v
  Div u v     -> eval env u / eval env v
  Pow u v     -> eval env u ** eval env v
  Sin u       -> sin  (eval env u)
  Cos u       -> cos  (eval env u)
  Tan u       -> tan  (eval env u)
  Exp u       -> exp  (eval env u)
  Log u       -> log  (eval env u)

-- | Pretty‐print with proper precedence, minimizing parentheses.
pretty :: Expr -> String
pretty = go 0 where
  prec :: Expr -> Int
  prec = \case
    Const _  -> 10; Var _ -> 10
    Neg _    -> 9; Sin{}  -> 9; Cos{} -> 9; Tan{} -> 9
    Exp{}    -> 9; Log{}  -> 9
    Pow{}    -> 8
    Mul{}    -> 7; Div{} -> 7
    Add{}    -> 6; Sub{} -> 6

  paren needed s = if needed then "("++s++")" else s

  go :: Int -> Expr -> String
  go _ (Const c) = if c<0 then "("++show c++")" else show c
  go _ (Var x)   = x
  go d (Neg u)   = paren (d>9) $ "-" ++ go 9 u
  go d (Add u v) = paren (d>6) $ go 6 u ++ " + " ++ go 6 v
  go d (Sub u v) = paren (d>6) $ go 6 u ++ " - " ++ go 6 v
  go d (Mul u v) = paren (d>7) $ go 7 u ++ " * " ++ go 7 v
  go d (Div u v) = paren (d>7) $ go 7 u ++ " / " ++ go 7 v
  go d (Pow u v) = paren (d>8) $ go 8 u ++ " ^ " ++ go 8 v
  go _ (Sin u)   = "sin(" ++ go 0 u ++ ")"
  go _ (Cos u)   = "cos(" ++ go 0 u ++ ")"
  go _ (Tan u)   = "tan(" ++ go 0 u ++ ")"
  go _ (Exp u)   = "exp(" ++ go 0 u ++ ")"
  go _ (Log u)   = "log(" ++ go 0 u ++ ")"
