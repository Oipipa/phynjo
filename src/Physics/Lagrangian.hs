{-# LANGUAGE LambdaCase #-}

module Physics.Lagrangian
  ( Coord(..)
  , LagM
  , defineCoord
  , timeDeriv
  , buildLagrangian
  , eulerLagrange
  , tDerivative
  ) where

import           SymbolicPhysics.SymbolicD
  ( Expr
  , var, constant
  , neg, add, sub, mul, divE, pow
  , sinE, cosE, tanE, expE, logE
  , deriv, simplify
  )
import qualified CAS.AST as C

newtype Coord = Coord
  { coordName :: String
  } deriving (Eq, Show)

newtype LagM a = LagM { runLagM :: ([Coord], a) }

instance Functor LagM where
  fmap f (LagM (cs, x)) = LagM (cs, f x)

instance Applicative LagM where
  pure x = LagM ([], x)
  LagM (cs1, f) <*> LagM (cs2, x) =
    LagM (cs1 ++ cs2, f x)

instance Monad LagM where
  return = pure
  LagM (cs1, x) >>= k =
    let LagM (cs2, y) = k x
    in  LagM (cs1 ++ cs2, y)

defineCoord :: String -> LagM Coord
defineCoord name = LagM ([Coord name], Coord name)

timeDeriv :: Coord -> LagM Expr
timeDeriv (Coord q) = pure $ var (q ++ "_dot")

buildLagrangian :: LagM Expr -> ([Coord], Expr)
buildLagrangian = runLagM

tDerivative :: [Coord] -> Expr -> Expr
tDerivative coords = \case
  C.Var x
    | any (\(Coord q) -> x == q ++ "_dot") coords ->
        let q = take (length x - length "_dot") x
        in var (q ++ "_ddot")
    | any (\(Coord q) -> x == q) coords ->
        var (x ++ "_dot")
    | otherwise ->
        constant 0

  C.Const _ ->
    constant 0

  C.Neg u   ->
    neg (tDerivative coords u)
  C.Add u v ->
    add (tDerivative coords u) (tDerivative coords v)
  C.Sub u v ->
    sub (tDerivative coords u) (tDerivative coords v)

  C.Mul u v ->
    add (mul (tDerivative coords u) v)
        (mul u (tDerivative coords v))
  C.Div u v ->
    divE
      ( sub (mul (tDerivative coords u) v)
            (mul u (tDerivative coords v)) )
      ( mul v v )

  C.Pow u v ->
    mul (pow u v)
        ( add (mul (tDerivative coords v) (logE u))
              (mul v (divE (tDerivative coords u) u)) )

  C.Sin u ->
    mul (tDerivative coords u) (cosE u)
  C.Cos u ->
    mul (tDerivative coords u) (neg (sinE u))
  C.Tan u ->
    mul (tDerivative coords u)
        ( add (constant 1) (pow (tanE u) (constant 2)) )

  C.Exp u ->
    mul (tDerivative coords u) (expE u)
  C.Log u ->
    divE (tDerivative coords u) u

eulerLagrange :: ([Coord], Expr) -> [(Coord, Expr)]
eulerLagrange (coords, lag) = map makeEq coords
 where
  makeEq c@(Coord q) =
    let qdot       = q ++ "_dot"
        dL_dq      = deriv q    lag
        dL_dqdot   = deriv qdot lag
        dt_dL_dqdot = tDerivative coords dL_dqdot
        residual   = simplify (sub dt_dL_dqdot dL_dq)
    in (c, residual)
