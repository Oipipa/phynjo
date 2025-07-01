-- src/SymbolicPhysics/SymbolicD.hs
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
import           Data.List       (partition)

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
  deriving (Eq, Ord, Show)

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
  Mul u v       -> add (mul (deriv x u) v) (mul u (deriv x v))
  Div u v       -> divE
                     ( sub (mul (deriv x u) v)
                           (mul u (deriv x v)) )
                     ( mul v v )
  Pow u (Const n) ->
                   mul (mul (Const n) (pow u (Const (n-1))))
                       (deriv x u)
  Pow u v       ->
                   let u' = deriv x u
                       v' = deriv x v
                   in mul (pow u v)
                          ( add (mul v' (logE u))
                                (mul v (divE u' u)) )
  Sin u         -> mul (deriv x u) (cosE u)
  Cos u         -> mul (deriv x u) (neg (sinE u))
  Tan u         -> mul (deriv x u)
                       ( add (Const 1) (pow (tanE u) (Const 2)) )
  Exp u         -> mul (deriv x u) (expE u)
  Log u         -> divE (deriv x u) u

-- | Fully simplify an expression:
--   1) expand products over sums,
--   2) one-pass local simplify & flatten,
--   3) combine like terms.
simplify :: Expr -> Expr
simplify = combineTerms . simp . expand
  where
    -- 1) Distribute Mul over Add/Sub
    expand :: Expr -> Expr
    expand = \case
      Add a b -> Add (expand a) (expand b)
      Sub a b -> Sub (expand a) (expand b)
      Mul a b ->
        let a' = expand a
            b' = expand b
        in case a' of
             Add x y -> expand (Add (Mul x b') (Mul y b'))
             Sub x y -> expand (Sub (Mul x b') (Mul y b'))
             _       -> case b' of
                          Add x y -> expand (Add (Mul a' x) (Mul a' y))
                          Sub x y -> expand (Sub (Mul a' x) (Mul a' y))
                          _       -> Mul a' b'
      Div a b -> Div (expand a) (expand b)
      Pow a b -> Pow (expand a) (expand b)
      Neg u   -> Neg (expand u)
      Sin u   -> Sin (expand u)
      Cos u   -> Cos (expand u)
      Tan u   -> Tan (expand u)
      Exp u   -> Exp (expand u)
      Log u   -> Log (expand u)
      atom    -> atom

    -- 2) One-pass local simplification & flattening
    simp :: Expr -> Expr
    simp = \case
      Neg (Const c)      -> Const (-c)
      Neg (Neg u)        -> simp u
      Neg u              -> Neg (simp u)

      Add u v            -> flattenAdd (simp u) (simp v)
      Sub u v            -> flattenAdd (simp u) (neg (simp v))

      Mul u v            -> flattenMul (simp u) (simp v)
      Div u v            -> simplifyDiv (simp u) (simp v)

      Pow u v            -> case (simp u, simp v) of
                              (Const a, Const b) -> Const (a ** b)
                              (_, Const 0)       -> Const 1
                              (Const 0, _)       -> Const 0
                              (u', v')           -> Pow u' v'

      Sin u              -> Sin (simp u)
      Cos u              -> Cos (simp u)
      Tan u              -> Tan (simp u)
      Exp u              -> Exp (simp u)
      Log u              -> Log (simp u)

      v@Var{}            -> v
      c@Const{}          -> c

    -- flatten Add/Sub, fold constants
    flattenAdd :: Expr -> Expr -> Expr
    flattenAdd x y =
      let terms = collectAdd x ++ collectAdd y
          (cs, vs) = partition isConst terms
          csum = sum (map getConst cs)
          non0 = [Const csum | csum /= 0]
      in case non0 ++ vs of
           []  -> Const 0
           [t] -> t
           ts   -> foldl1 Add ts

    -- flatten Mul/Div, fold constants
    flattenMul :: Expr -> Expr -> Expr
    flattenMul x y =
      let facts = collectMul x ++ collectMul y
          (cs, vs) = partition isConst facts
          prod = product (map getConst cs)
      in if prod == 0 then Const 0
         else
           let non1 = [Const prod | prod /= 1]
               allf = non1 ++ vs
           in case allf of
                []  -> Const 1
                [f] -> f
                fs   -> foldl1 Mul fs

    simplifyDiv :: Expr -> Expr -> Expr
    simplifyDiv u v = case (u, v) of
      (Const a, Const b) -> Const (a / b)
      (_, Const 1)       -> u
      (Const 0, _)       -> Const 0
      _                  -> Div u v

    collectAdd :: Expr -> [Expr]
    collectAdd = \case
      Add a b -> collectAdd a ++ collectAdd b
      Sub a b -> collectAdd a ++ [neg b]
      Neg u   -> [neg u]
      e       -> [e]

    collectMul :: Expr -> [Expr]
    collectMul = \case
      Mul a b -> collectMul a ++ collectMul b
      Div a b -> collectMul a ++ [Div (Const 1) b]
      e       -> [e]

    isConst :: Expr -> Bool
    isConst (Const _) = True
    isConst _         = False

    getConst :: Expr -> Double
    getConst (Const c) = c
    getConst _         = 1

    -- 3) Combine like terms in sums, re-flatten products
    combineTerms :: Expr -> Expr
    combineTerms = \case
      Add a b ->
        let terms  = collectAdd a ++ collectAdd b
            (cs, vs) = partition isConst (map combineTerms terms)
            csum   = sum (map getConst cs)
            mp     = M.fromListWith (+) [ (v,1::Double) | v <- vs ]
            merged = [ if n==1 then v else Mul (Const n) v
                     | (v,n) <- M.toList mp ]
            allT   = [Const csum | csum /= 0] ++ merged
        in case allT of
             []  -> Const 0
             [t] -> t
             ts   -> foldl1 Add ts

      Mul a b ->
        let facts = collectMul a ++ collectMul b
        in flattenMulList (map combineTerms facts)

      Neg u   -> Neg (combineTerms u)
      Sub u v -> combineTerms (Add (combineTerms u) (Neg (combineTerms v)))
      Div u v -> simplifyDiv (combineTerms u) (combineTerms v)
      Pow u v -> Pow (combineTerms u) (combineTerms v)
      Sin u   -> Sin (combineTerms u)
      Cos u   -> Cos (combineTerms u)
      Tan u   -> Tan (combineTerms u)
      Exp u   -> Exp (combineTerms u)
      Log u   -> Log (combineTerms u)
      atom    -> atom

    flattenMulList :: [Expr] -> Expr
    flattenMulList fs =
      let (cs, vs) = partition isConst fs
          prod     = product (map getConst cs)
      in if prod == 0 then Const 0
         else
           let non1 = [Const prod | prod /= 1]
               allf = non1 ++ vs
           in case allf of
                []  -> Const 1
                [f] -> f
                xs   -> foldl1 Mul xs

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

-- | Pretty‐print with minimal parentheses.
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

  go _ (Const c) = if c < 0 then "("++show c++")" else show c
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
