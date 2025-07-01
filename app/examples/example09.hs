-- double pendulum
{-# LANGUAGE LambdaCase #-}
module Main where

import SymbolicPhysics.SymbolicD
  ( Expr(..)
  , var, constant
  , neg, add, sub, mul, divE, pow
  , sinE, cosE
  , deriv, simplify, eval
  )
import Physics.Lagrangian
  ( Coord(..)
  , buildLagrangian
  , defineCoord
  , timeDeriv
  , eulerLagrange
  )
import qualified Data.Map.Strict as M
import Text.Printf (printf)

----------------------------------------------------------------------
-- 1) Symbolic setup: Lagrangian and Euler–Lagrange residuals
----------------------------------------------------------------------

-- Build the double‐pendulum Lagrangian L(q, q̇)
doublePendulumL :: ([Coord], Expr)
doublePendulumL = buildLagrangian $ do
  θ1  <- defineCoord "θ1"; θ1d <- timeDeriv θ1
  θ2  <- defineCoord "θ2"; θ2d <- timeDeriv θ2

  let m1 = constant 1.0
      m2 = constant 1.0
      l1 = constant 1.0
      l2 = constant 1.0
      g  = constant 9.81

      -- Kinetic energy of mass 1
      t1 = mul (constant 0.5) (mul m1 (mul l1 (mul l1 (mul θ1d θ1d))))
      -- Kinetic energy of mass 2 (with full 2D velocity)
      vx2 = add (mul l1 θ1d `mul` cosE (var "θ1"))
                (mul l2 θ2d `mul` cosE (var "θ2"))
      vy2 = add (mul l1 θ1d `mul` sinE (var "θ1"))
                (mul l2 θ2d `mul` sinE (var "θ2"))
      t2  = mul (constant 0.5) (mul m2 (add (mul vx2 vx2) (mul vy2 vy2)))

      -- Potential energy (zero at y=0)
      v1 = mul m1 (mul g (mul l1 (sub (constant 1) (cosE (var "θ1")))))
      v2 = mul m2
           (mul g
             (add (mul l1 (sub (constant 1) (cosE (var "θ1"))))
                  (mul l2 (sub (constant 1) (cosE (var "θ2"))))))

  return (sub (add t1 t2) (add v1 v2))

-- Extract coords, L, and residuals
coords@[Coord _, Coord _] = fst doublePendulumL
lag                       = snd doublePendulumL
eqns :: [(Coord, Expr)]
eqns = eulerLagrange (coords, lag)

res1, res2 :: Expr
res1 = snd (eqns !! 0)
res2 = snd (eqns !! 1)

-- Coefficients for θ̈1, θ̈2 in each residual
c11, c12, c21, c22, c1_expr, c2_expr :: Expr
c11    = simplify $ deriv "θ1_ddot" res1
c12    = simplify $ deriv "θ2_ddot" res1
c21    = simplify $ deriv "θ1_ddot" res2
c22    = simplify $ deriv "θ2_ddot" res2

-- Remainder terms: residual minus θ̈-parts
c1_expr = simplify $ sub res1
                       ( add (mul c11 (var "θ1_ddot"))
                             (mul c12 (var "θ2_ddot")) )
c2_expr = simplify $ sub res2
                       ( add (mul c21 (var "θ1_ddot"))
                             (mul c22 (var "θ2_ddot")) )

----------------------------------------------------------------------
-- 2) Numeric helpers: solve for accelerations & RK4 integrator
----------------------------------------------------------------------

type State = (Double,Double,Double,Double)  -- (θ1, ω1, θ2, ω2)

-- Solve 2×2 system for θ̈1, θ̈2 given current state
computeAccel :: State -> (Double,Double)
computeAccel (θ1,ω1,θ2,ω2) =
  let env = M.fromList
        [ ("θ1",     θ1)
        , ("θ2",     θ2)
        , ("θ1_dot", ω1)
        , ("θ2_dot", ω2)
        ]
      [m11,m12,m21,m22] = map (eval env) [c11,c12,c21,c22]
      [r1, r2]          = map (eval env) [c1_expr, c2_expr]
      det = m11*m22 - m12*m21
      a1  = (-m22*r1 + m12*r2) / det
      a2  = ( m21*r1 - m11*r2) / det
  in (a1,a2)

-- Classic RK4 for 4D state
rk4Step :: (State->State) -> Double -> State -> State
rk4Step f h s0 = ( x0 + h*(k11+2*k21+2*k31+k41)/6
                 , v0 + h*(k12+2*k22+2*k32+k42)/6
                 , y0 + h*(k13+2*k23+2*k33+k43)/6
                 , w0 + h*(k14+2*k24+2*k34+k44)/6
                 )
  where
    (x0,v0,y0,w0) = s0
    (k11,k12,k13,k14) = f s0
    s1 = (x0 + 0.5*h*k11, v0 + 0.5*h*k12
         ,y0 + 0.5*h*k13, w0 + 0.5*h*k14)
    (k21,k22,k23,k24) = f s1
    s2 = (x0 + 0.5*h*k21, v0 + 0.5*h*k22
         ,y0 + 0.5*h*k23, w0 + 0.5*h*k24)
    (k31,k32,k33,k34) = f s2
    s3 = (x0 + h*k31, v0 + h*k32
         ,y0 + h*k33, w0 + h*k34)
    (k41,k42,k43,k44) = f s3

-- Derivative of full state
f :: State -> State
f (θ1,ω1,θ2,ω2) =
  let (a1,a2) = computeAccel (θ1,ω1,θ2,ω2)
  in (ω1, a1, ω2, a2)

----------------------------------------------------------------------
-- 3) Simulation & CSV output
----------------------------------------------------------------------

dt    = 0.01
tmax  = 20.0
steps = floor (tmax / dt)

initial :: State
initial = (1.0, 0.0, 0.5, 0.0)

simulate :: [State]
simulate = take (steps+1) $ iterate (rk4Step f dt) initial

main :: IO ()
main = do
  putStrLn "t,θ1,ω1,θ2,ω2"
  let times = [0, dt .. tmax]
  mapM_ (\(t,(θ1,ω1,θ2,ω2)) ->
            printf "%.4f,%.6f,%.6f,%.6f,%.6f\n"
                   t θ1 ω1 θ2 ω2)
        (zip times simulate)
