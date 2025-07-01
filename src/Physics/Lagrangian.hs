{-# LANGUAGE LambdaCase #-}

-- | Symbolic Lagrangian front‐end: declare generalized coordinates,
--   build a Lagrangian L(q, q̇), and automatically derive the
--   Euler–Lagrange equations d/dt(∂L/∂q̇) − ∂L/∂q = 0.
module Physics.Lagrangian
  ( Coord(..)
  , LagM
  , defineCoord
  , timeDeriv
  , buildLagrangian
  , eulerLagrange
  , tDerivative
  ) where

import           Data.List       (isSuffixOf)
import           Physics.Symbolic
  ( Expr(..)
  , var
  , constant
  , neg
  , add, sub, mul, divE, pow
  , sinE, cosE, tanE, expE, logE
  , deriv, simplify
  )

----------------------------------------------------------------------  
-- | A generalized coordinate, identified by a name.
----------------------------------------------------------------------
newtype Coord = Coord
  { coordName :: String
  } deriving (Eq, Show)

----------------------------------------------------------------------  
-- | Lagrangian‐builder monad collects all declared coordinates.
----------------------------------------------------------------------
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

----------------------------------------------------------------------  
-- | Declare a new generalized coordinate q.
----------------------------------------------------------------------
defineCoord :: String -> LagM Coord
defineCoord name = LagM ([Coord name], Coord name)

----------------------------------------------------------------------  
-- | Refer to the time‐derivative ˙q as a new symbolic variable "q_dot".
----------------------------------------------------------------------
timeDeriv :: Coord -> LagM Expr
timeDeriv (Coord q) = pure $ var (q ++ "_dot")

----------------------------------------------------------------------  
-- | Run the builder to get the list of coordinates and the Lagrangian Expr.
----------------------------------------------------------------------
buildLagrangian :: LagM Expr -> ([Coord], Expr)
buildLagrangian = runLagM

----------------------------------------------------------------------  
-- | Symbolic time‐derivative:
--   q ↦ q_dot, q_dot ↦ q_ddot, others ↦ 0,
--   with full chain‐rule on subexpressions.
----------------------------------------------------------------------
tDerivative :: [Coord] -> Expr -> Expr
tDerivative coords = \case
  Var x
    | any (\(Coord q) -> x == q ++ "_dot") coords ->
        let q = take (length x - length "_dot") x
        in var (q ++ "_ddot")
    | any (\(Coord q) -> x == q) coords ->
        var (x ++ "_dot")
    | otherwise -> Const 0

  Const _ -> Const 0

  Neg u   -> neg (tDerivative coords u)
  Add u v -> add (tDerivative coords u) (tDerivative coords v)
  Sub u v -> sub (tDerivative coords u) (tDerivative coords v)

  Mul u v ->
    add (mul (tDerivative coords u) v)
        (mul u (tDerivative coords v))

  Div u v ->
    divE
      ( sub (mul (tDerivative coords u) v)
            (mul u (tDerivative coords v)) )
      ( mul v v )

  Pow u v ->
    let u' = tDerivative coords u
        v' = tDerivative coords v
    in mul (pow u v)
           ( add (mul v' (logE u))
                (mul v  (divE u' u)) )

  Sin u -> mul (tDerivative coords u) (cosE u)
  Cos u -> mul (tDerivative coords u) (neg (sinE u))
  Tan u -> mul (tDerivative coords u)
               ( add (constant 1) (pow (tanE u) (constant 2)) )
  Exp u -> mul (tDerivative coords u) (expE u)
  Log u -> divE (tDerivative coords u) u

----------------------------------------------------------------------  
-- | Derive the Euler–Lagrange equations:
--     d/dt(∂L/∂q̇) − ∂L/∂q = 0
--   Returns [(Coord, residualExpr)] for each coordinate.
----------------------------------------------------------------------
eulerLagrange :: ([Coord], Expr) -> [(Coord, Expr)]
eulerLagrange (coords, lag) = map makeEq coords
 where
  makeEq c@(Coord q) =
    let qdot        = q ++ "_dot"
        dL_dq        = deriv q    lag
        dL_dqdot     = deriv qdot lag
        dt_dL_dqdot  = tDerivative coords dL_dqdot
        residual     = simplify (sub dt_dL_dqdot dL_dq)
    in (c, residual)
