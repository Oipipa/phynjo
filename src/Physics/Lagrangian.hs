{-# LANGUAGE LambdaCase #-}

module Physics.Lagrangian
  ( Coord(..)
  , LagM
  , defineCoord
  , timeDeriv
  , buildLagrangian
  , eulerLagrange
  , tDerivative
  , q, qd, qdd
  ) where

import           CAS.AST               as C
import           CAS.Differentiate     (differentiate)
import           CAS.Simplify          (simplify, simplifyFix)

-- ---------------------------------------------------------------------
-- Coordinates and tiny builder monad
-- ---------------------------------------------------------------------

newtype Coord = Coord { coordName :: String }
  deriving (Eq, Ord, Show)

-- Collect declared coordinates alongside the built expression
newtype LagM a = LagM { runLagM :: ([Coord], a) }

instance Functor LagM where
  fmap f (LagM (cs, x)) = LagM (cs, f x)

instance Applicative LagM where
  pure x = LagM ([], x)
  LagM (cs1, f) <*> LagM (cs2, x) = LagM (cs1 ++ cs2, f x)

instance Monad LagM where
  return = pure
  LagM (cs1, x) >>= k =
    let LagM (cs2, y) = k x
    in  LagM (cs1 ++ cs2, y)

defineCoord :: String -> LagM Coord
defineCoord name = LagM ([Coord name], Coord name)

-- handy constructors for q, q̇, q̈ variables
q   :: Coord -> Expr
q   (Coord s) = Var s

qd  :: Coord -> Expr
qd  (Coord s) = Var (s ++ "_dot")

qdd :: Coord -> Expr
qdd (Coord s) = Var (s ++ "_ddot")

-- for building L(q, q̇, t) with the mini-DSL
timeDeriv :: Coord -> LagM Expr
timeDeriv c = pure (qd c)

buildLagrangian :: LagM Expr -> ([Coord], Expr)
buildLagrangian = runLagM

-- ---------------------------------------------------------------------
-- Total time derivative d/dt acting on Expr[q, q̇]
-- ---------------------------------------------------------------------

tDerivative :: [Coord] -> Expr -> Expr
tDerivative coords = go
  where
    isQdotName x = any (\(Coord s) -> x == s ++ "_dot") coords
    isQName   x  = any (\(Coord s) -> x == s)           coords

    go :: Expr -> Expr
    go = \case
      C.Var x
        | isQdotName x ->
            let base = take (length x - length "_dot") x
            in C.Var (base ++ "_ddot")
        | isQName x ->
            C.Var (x ++ "_dot")
        | otherwise ->
            C.Const 0

      C.Const _   -> C.Const 0

      C.Neg u     -> simplify (C.Neg (go u))
      C.Add u v   -> simplify (C.Add (go u) (go v))
      C.Sub u v   -> simplify (C.Sub (go u) (go v))

      C.Mul u v   -> simplify (C.Add (C.Mul (go u) v)
                                      (C.Mul u (go v)))
      C.Div u v   -> simplify (C.Div (C.Sub (C.Mul (go u) v)
                                            (C.Mul u (go v)))
                                      (C.Mul v v))

      C.Pow u v   ->
        -- d/dt u^v = u^v * ( v' * log u + v * u'/u )
        let du = go u
            dv = go v
        in simplify $
             C.Mul (C.Pow u v)
                   (C.Add (C.Mul dv (C.Log u))
                          (C.Mul v  (C.Div du u)))

      C.Sin u     -> simplify (C.Mul (go u) (C.Cos u))
      C.Cos u     -> simplify (C.Neg (C.Mul (go u) (C.Sin u)))
      C.Tan u     ->
        -- sec^2(u) = 1 + tan^2(u)
        let du = go u
        in  simplify (C.Mul du (C.Add (C.Const 1) (C.Pow (C.Tan u) (C.Const 2))))

      C.Exp u     -> simplify (C.Mul (go u) (C.Exp u))
      C.Log u     -> simplify (C.Div (go u) u)

-- ---------------------------------------------------------------------
-- Euler–Lagrange equations
-- ---------------------------------------------------------------------

dBy :: String -> Expr -> Expr
dBy name e = simplify (differentiate name e)

-- For each coordinate q:   d/dt (∂L/∂q̇) − ∂L/∂q = 0
eulerLagrange :: ([Coord], Expr) -> [(Coord, Expr)]
eulerLagrange (coords, lag) = map elOne coords
  where
    elOne c@(Coord s) =
      let dL_dq        = dBy s             lag
          dL_dqdot     = dBy (s ++ "_dot") lag
          dt_dL_dqdot  = tDerivative coords dL_dqdot
          residual     = simplifyFix (C.Sub dt_dL_dqdot dL_dq)
      in (c, residual)
