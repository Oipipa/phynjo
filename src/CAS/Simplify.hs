module CAS.Simplify
  ( simplify
  , simplifyAdd
  , simplifySub
  , simplifyFix
  ) where

import           CAS.AST
import           Data.List            (foldl', sort)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict as M
import           Data.Ratio

-- ---------------------------------------------------------------------
-- Basics
-- ---------------------------------------------------------------------

zero, one :: Expr
zero = Const 0
one  = Const 1

isZero, isOne :: Expr -> Bool
isZero (Const r) = r == 0
isZero _         = False
isOne  (Const r) = r == 1
isOne  _         = False

-- ---------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------

-- One-pass recursive simplifier with local rules
simplify :: Expr -> Expr
simplify (Add a b) = simplifyAdd (simplify a) (simplify b)
simplify (Sub a b) = simplifySub (simplify a) (simplify b)
simplify (Mul a b) = simplifyMul (simplify a) (simplify b)
simplify (Div a b) = simplifyDiv (simplify a) (simplify b)
simplify (Pow a b) = simplifyPow (simplify a) (simplify b)
simplify (Neg a)   = simplifyNeg (simplify a)

-- small identities on common unary ops
simplify (Sin a) = case simplify a of
  x | isZero x  -> zero
  Neg y         -> simplifyNeg (Sin y)   -- sin(-x) = -sin x
  x             -> Sin x
simplify (Cos a) = case simplify a of
  x | isZero x  -> one
  Neg y         -> Cos y                 -- cos(-x) = cos x
  x             -> Cos x
simplify (Tan a) = case simplify a of
  x | isZero x  -> zero
  x             -> Tan x
simplify (Exp a) = case simplify a of
  x | isZero x  -> one
  x             -> Exp x
simplify (Log a) = case simplify a of
  x | isOne x   -> zero
  x             -> Log x

simplify e = e

-- Iterate simplify + a trig-combining pass to a fixed point
simplifyFix :: Expr -> Expr
simplifyFix = go 0
  where
    go :: Int -> Expr -> Expr
    go k e
      | k > 64    = e
      | otherwise =
          let e1 = simplify e
              e2 = simplifyTrigOnce e1
          in if e2 == e
                then e
                else go (k+1) e2

-- ---------------------------------------------------------------------
-- Addition / Subtraction
-- ---------------------------------------------------------------------

-- Combine like terms, flatten sums, fold constants
simplifyAdd :: Expr -> Expr -> Expr
simplifyAdd x y = rebuildSum (collectSum (flattenSum x ++ flattenSum y))

simplifySub :: Expr -> Expr -> Expr
simplifySub x y = simplifyAdd x (simplifyNeg y)

-- Flatten a sum to a list of terms (Sub and Neg included)
flattenSum :: Expr -> [Expr]
flattenSum (Add a b) = flattenSum a ++ flattenSum b
flattenSum (Sub a b) = flattenSum a ++ map simplifyNeg (flattenSum b)
flattenSum (Neg a)   = map simplifyNeg (flattenSum a)
flattenSum e         = [e]

-- Represent a term as (coefficient, base-without-leading-Const/Neg)
splitCoeff :: Expr -> (Rational, Expr)
splitCoeff (Const c) = (c, one)
splitCoeff (Neg e)   = let (c,b) = splitCoeff e in (-c, b)
splitCoeff (Mul a b) =
  let (ca,ba) = splitCoeff a
      (cb,bb) = splitCoeff b
  in (ca * cb, mulNorm ba bb)
splitCoeff e = (1, e)

-- Normalize product core: flatten and sort factors, drop 1
mulNorm :: Expr -> Expr -> Expr
mulNorm a b = mkMulList (factors a ++ factors b)
  where
    factors :: Expr -> [Expr]
    factors (Mul u v) = factors u ++ factors v
    factors e'
      | isOne e'     = []
      | otherwise    = [e']

-- Group like bases in a sum and add coefficients
collectSum :: [Expr] -> Map Expr Rational
collectSum = foldl' step M.empty
  where
    step acc term =
      let (c,b) = splitCoeff term
      in if c == 0 then acc else M.insertWith (+) (canon b) c acc

    -- simple canonicalization for product bases: sorted factors
    canon :: Expr -> Expr
    canon e = case e of
      Mul u v -> mkMulList (sort (flattenMul (Mul u v)))
      _       -> e

    flattenMul :: Expr -> [Expr]
    flattenMul (Mul u v) = flattenMul u ++ flattenMul v
    flattenMul t
      | isOne t         = []
      | otherwise       = [t]

rebuildSum :: Map Expr Rational -> Expr
rebuildSum m =
  let (c, rest) = M.updateLookupWithKey (\_ _ -> Nothing) one m
      constSum  = maybe 0 id c
      terms     = [ termFrom (coef, base)
                  | (base, coef) <- M.toList rest
                  , coef /= 0
                  ]
      allTerms  = (if constSum /= 0 then [Const constSum] else []) ++ terms
  in case allTerms of
       []  -> zero
       [t] -> t
       ts  -> mkAddList ts

termFrom :: (Rational, Expr) -> Expr
termFrom (c, b)
  | c == 0    = zero
  | b == one  = Const c
  | c == 1    = b
  | c == (-1) = simplifyNeg b
  | otherwise = Mul (Const c) b

-- ---------------------------------------------------------------------
-- Multiplication / Division / Power / Negation
-- ---------------------------------------------------------------------

-- Distribute product over sums on BOTH sides so trig combiner can fire
simplifyMul :: Expr -> Expr -> Expr
simplifyMul (Add a b) t = simplifyAdd (simplifyMul a t) (simplifyMul b t)
simplifyMul (Sub a b) t = simplifySub (simplifyMul a t) (simplifyMul b t)
simplifyMul t (Add a b) = simplifyAdd (simplifyMul t a) (simplifyMul t b)
simplifyMul t (Sub a b) = simplifySub (simplifyMul t a) (simplifyMul t b)

-- Core multiply with coeff folding and zero/one rules
simplifyMul (Const x) (Const y) = Const (x * y)
simplifyMul x y
  | isZero x || isZero y = zero
  | isOne x              = y
  | isOne y              = x
  | otherwise =
      let (c, b) = splitCoeff (Mul x y)
      in if c == 0 then zero
         else case b of
           t | t == one   -> Const c
             | c == 1     -> t
             | c == (-1)  -> simplifyNeg t
             | otherwise  -> Mul (Const c) t

simplifyDiv :: Expr -> Expr -> Expr
simplifyDiv (Const x) (Const y)
  | y == 0    = Div (Const x) (Const y)   -- leave as-is, avoid crash
  | otherwise = Const (x / y)
simplifyDiv x y
  | isZero x  = zero
  | isOne  y  = x
  | otherwise =
      let (cx, bx) = splitCoeff x
          (cy, by) = splitCoeff y
          c        = cx / cy
      in if bx == by && bx /= one
           then if c == 1 then one else Const c
           else
             let core = if by == one then bx else Div bx by
             in case compare c 1 of
                  EQ -> core
                  _  -> simplifyMul (Const c) core

simplifyPow :: Expr -> Expr -> Expr
-- exponents
simplifyPow _         (Const 0)         = one
simplifyPow x         (Const 1)         = x
simplifyPow (Const 0) (Const e) | e > 0 = zero
simplifyPow (Const 1) _                 = one

-- constant base, integer exponent (incl negative)
simplifyPow (Const b) (Const e)
  | denominator e == 1 =
      let k = numerator e
      in if k >= 0
           then Const ((numerator b ^ k) % (denominator b ^ k))
           else if numerator b == 0
                  then Pow (Const b) (Const e)  -- leave 0^neg un-evaluated
                  else Const ((denominator b ^ negate k) % (numerator b ^ negate k))

-- symbolic base, integer exponents 0 and 1 already handled
simplifyPow x (Const e)
  | denominator e == 1 =
      let k = numerator e
      in if k == 0 then one
         else if k == 1 then x
         else Pow x (Const e)

-- a^(-1) -> 1/a (covers non-integers via explicit check too)
simplifyPow x (Const e)
  | e == (-1) = simplifyDiv one x

simplifyPow x y = Pow x y

simplifyNeg :: Expr -> Expr
simplifyNeg (Const x) = Const (negate x)
simplifyNeg (Neg x)   = x
simplifyNeg x         = Neg x

-- ---------------------------------------------------------------------
-- Trig combiner (sum-to-difference identities)
-- ---------------------------------------------------------------------

-- Helpers to detect trig heads
isSin :: Expr -> Maybe Expr
isSin (Sin a) = Just a
isSin _       = Nothing

isCos :: Expr -> Maybe Expr
isCos (Cos a) = Just a
isCos _       = Nothing

-- Split a term into rational coeff and factor list (no leading Const/Neg)
coeffAndFactors :: Expr -> (Rational, [Expr])
coeffAndFactors e =
  let (c, core) = splitCoeff e
  in (c, flatten core)
  where
    flatten (Mul a b) = flatten a ++ flatten b
    flatten t
      | isOne t      = []
      | otherwise    = [t]

-- Partition factor list into sin-args, cos-args, and others
partitionTrig :: [Expr] -> ([Expr],[Expr],[Expr])
partitionTrig = go [] [] []
  where
    go ss cs os []     = (reverse ss, reverse cs, reverse os)
    go ss cs os (f:fs) = case (isSin f, isCos f) of
      (Just u, _) -> go (u:ss) cs os fs
      (_, Just u) -> go ss (u:cs) os fs
      _           -> go ss cs (f:os) fs

-- Try to combine one pair of terms in a sum using identities:
--   A) sin u sin v  + cos u cos v = cos(u - v)
--   B) cos u sin v  - sin u cos v = sin(v - u)
--   C) sin u cos v  - sin v cos u = sin(u - v)
combineTrigPair :: [Expr] -> Maybe (Expr, (Int,Int))
combineTrigPair terms = outer 0
  where
    outer i
      | i >= length terms = Nothing
      | otherwise =
          let (ci, fi) = coeffAndFactors (terms !! i)
              (sSi, cSi, oSi) = partitionTrig fi
          in if length sSi + length cSi == 2
               then inner i ci sSi cSi oSi (i+1)
               else outer (i+1)

    inner i ci sSi cSi oSi j
      | j >= length terms = outer (i+1)
      | otherwise =
          let (cj, fj) = coeffAndFactors (terms !! j)
              (sSj, cSj, oSj) = partitionTrig fj
          in if sort oSi /= sort oSj
               then inner i ci sSi cSi oSi (j+1)
               else
                 let -- Case A
                     tryA =
                       case (sort sSi, sort cSi, sort sSj, sort cSj, ci == cj) of
                         ([u,v], [], [], [u',v'], True)
                           | [u,v] == [u',v'] ->
                               let core = mkMulList oSi
                                   t    = if core == one
                                            then Cos (Sub u v)
                                            else Mul core (Cos (Sub u v))
                               in Just (termFrom (ci, t), (i,j))
                         _ -> Nothing
                     -- Case B
                     tryB =
                       case (sSi, cSi, sSj, cSj, ci == negate cj) of
                         ([], [u], [u'], [v], True)
                           | u == u' ->
                               let core = mkMulList oSi
                                   t    = if core == one
                                            then Sin (Sub v u)
                                            else Mul core (Sin (Sub v u))
                               in Just (termFrom (ci, t), (i,j))
                         ([u'], [v], [u], [], True)
                           | u == u' ->
                               let core = mkMulList oSi
                                   t    = if core == one
                                            then Sin (Sub v u)
                                            else Mul core (Sin (Sub v u))
                               in Just (termFrom (cj, t), (i,j))
                         _ -> Nothing
                     -- Case C
                     tryC =
                       case (sSi, cSi, sSj, cSj, ci == negate cj) of
                         ([u], [v], [v'], [u'], True)
                           | u == u' && v == v' ->
                               let core = mkMulList oSi
                                   t    = if core == one
                                            then Sin (Sub u v)
                                            else Mul core (Sin (Sub u v))
                               in Just (termFrom (ci, t), (i,j))
                         ([v], [u], [u'], [v'], True)
                           | u == u' && v == v' ->
                               let core = mkMulList oSi
                                   t    = if core == one
                                            then Sin (Sub v u)
                                            else Mul core (Sin (Sub v u))
                               in Just (termFrom (ci, t), (i,j))
                         _ -> Nothing

                     orElse :: Maybe a -> Maybe a -> Maybe a
                     orElse (Just x) _ = Just x
                     orElse Nothing  y = y

                 in case tryA `orElse` tryB `orElse` tryC of
                      Just r  -> Just r
                      Nothing -> inner i ci sSi cSi oSi (j+1)

-- Apply trig combining once to a flattened sum, otherwise return input
simplifyTrigOnce :: Expr -> Expr
simplifyTrigOnce e =
  let ts = flattenSum e
  in case combineTrigPair ts of
       Nothing -> e
       Just (tNew, (i,j)) ->
         let keep k _ = k /= i && k /= j
             rest     = [ t | (k,t) <- zip [0..] ts, keep k t ]
             sum'     = mkAddList (tNew : rest)
         in simplify sum'

-- ---------------------------------------------------------------------
-- Builders (flat and ordered)
-- ---------------------------------------------------------------------

mkAddList :: [Expr] -> Expr
mkAddList []  = zero
mkAddList [x] = x
mkAddList xs  = foldl1 Add xs

mkMulList :: [Expr] -> Expr
mkMulList []  = one
mkMulList [x] = x
mkMulList xs  =
  let xs' = filter (not . isOne) xs
  in case xs' of
       []  -> one
       [t] -> t
       ts  -> foldl1 Mul (sort ts)
