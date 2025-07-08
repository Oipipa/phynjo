{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                   (forM_)
import           Physics.Lagrangian              (Coord(..), defineCoord, timeDeriv, buildLagrangian, eulerLagrange)
import           SymbolicPhysics.SymbolicD       (Expr, constant, var, sinE, cosE, add, sub, mul)
import           SymbolicPhysics.PrettyEL        (prettyEL)

-- | Build the Lagrangian of a planar double pendulum.
doublePendulum :: ([Coord], Expr)
doublePendulum = buildLagrangian $ do
  -- Define generalized coordinates
  theta1  <- defineCoord "theta1"
  theta2  <- defineCoord "theta2"
  -- Create Expr handles for the coordinates
  let theta1e = var (coordName theta1)
      theta2e = var (coordName theta2)
  -- Time derivatives
  theta1d <- timeDeriv theta1
  theta2d <- timeDeriv theta2

  let m1 = constant 1
      m2 = constant 1
      l1 = constant 1
      l2 = constant 1
      g  = constant 9.81

      -- Kinetic energy of mass 1
      t1  = mul (constant 0.5) (mul m1 (mul l1 (mul l1 (mul theta1d theta1d))))
      -- Velocities of mass 2
      vx2 = add (mul l1 (mul (cosE theta1e) theta1d))
                (mul l2 (mul (cosE theta2e) theta2d))
      vy2 = add (mul l1 (mul (sinE theta1e) theta1d))
                (mul l2 (mul (sinE theta2e) theta2d))
      -- Kinetic energy of mass 2
      t2  = mul (constant 0.5) (mul m2 (add (mul vx2 vx2) (mul vy2 vy2)))
      -- Potential energies
      v1  = mul m1 (mul g (mul l1 (sub (constant 1) (cosE theta1e))))
      v2  = mul m2 (mul g
             (add (mul l1 (sub (constant 1) (cosE theta1e)))
                  (mul l2 (sub (constant 1) (cosE theta2e)))))

  -- Lagrangian L = T1 + T2 - V1 - V2
  return (sub (add t1 t2) (add v1 v2))

main :: IO ()
main = do
  let (coords, lag) = doublePendulum
      raws           = eulerLagrange (coords, lag)
      names          = [ q | Coord q <- coords ]

  putStrLn "Double pendulum equations of motion:"
  forM_ raws $ \(Coord theta, expr) ->
    putStrLn (prettyEL names (theta, expr))
