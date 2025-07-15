{-# LANGUAGE LambdaCase #-}

module SymbolicPhysics.SymbolicD
  ( Expr
  , var, constant
  , neg, add, sub, mul, divE, pow
  , sinE, cosE, tanE, expE, logE
  , deriv
  , simplify
  , eval
  , pretty
  ) where

import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as M
import           Data.Maybe         (fromMaybe)
import           Data.Ratio         (Rational)
import qualified CAS.AST            as C
import qualified CAS.Differentiate  as D
import qualified CAS.Simplify       as S
import qualified CAS.PrettyPrinter  as PP

type Expr = C.Expr

-- Smart constructors
var      :: String -> Expr
var      = C.Var

constant :: Double -> Expr
constant = C.Const . toRational

neg  :: Expr -> Expr
neg  = C.Neg

add  :: Expr -> Expr -> Expr
add  = C.Add

sub  :: Expr -> Expr -> Expr
sub  = C.Sub

mul  :: Expr -> Expr -> Expr
mul  = C.Mul

divE :: Expr -> Expr -> Expr
divE = C.Div

pow  :: Expr -> Expr -> Expr
pow  = C.Pow

sinE :: Expr -> Expr
sinE = C.Sin

cosE :: Expr -> Expr
cosE = C.Cos

tanE :: Expr -> Expr
tanE = C.Tan

expE :: Expr -> Expr
expE = C.Exp

logE :: Expr -> Expr
logE = C.Log

-- | Symbolic derivative via our CAS engine
deriv :: String -> Expr -> Expr
deriv v expr = S.simplify (D.differentiate v expr)

-- | Full symbolic simplification
simplify :: Expr -> Expr
simplify = S.simplify

-- | Numeric evaluation (unbound vars → 0)
eval :: Map String Double -> Expr -> Double
eval env = go where
  go :: Expr -> Double
  go = \case
    C.Var x   -> fromMaybe 0 (M.lookup x env)
    C.Const r -> fromRational r
    C.Neg u   -> negate (go u)
    C.Add u v -> go u + go v
    C.Sub u v -> go u - go v
    C.Mul u v -> go u * go v
    C.Div u v -> go u / go v
    C.Pow u v -> go u ** go v
    C.Sin u   -> sin  (go u)
    C.Cos u   -> cos  (go u)
    C.Tan u   -> tan  (go u)
    C.Exp u   -> exp  (go u)
    C.Log u   -> log  (go u)

-- | Pretty‐print
pretty :: Expr -> String
pretty = PP.pretty
