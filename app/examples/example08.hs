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
  θ1  <- defineCoord "θ1"
  θ2  <- defineCoord "θ2"
  -- Create Expr handles for the coordinates
  let θ1e = var (coordName θ1)
      θ2e = var (coordName θ2)
  -- Time derivatives
  θ1d <- timeDeriv θ1
  θ2d <- timeDeriv θ2

  let m1 = constant 1
      m2 = constant 1
      ℓ1 = constant 1
      ℓ2 = constant 1
      g  = constant 9.81

      -- Kinetic energy of mass 1
      t1  = mul (constant 0.5) (mul m1 (mul ℓ1 (mul ℓ1 (mul θ1d θ1d))))
      -- Velocities of mass 2
      vx2 = add (mul ℓ1 (mul (cosE θ1e) θ1d))
                (mul ℓ2 (mul (cosE θ2e) θ2d))
      vy2 = add (mul ℓ1 (mul (sinE θ1e) θ1d))
                (mul ℓ2 (mul (sinE θ2e) θ2d))
      -- Kinetic energy of mass 2
      t2  = mul (constant 0.5) (mul m2 (add (mul vx2 vx2) (mul vy2 vy2)))
      -- Potential energies
      v1  = mul m1 (mul g (mul ℓ1 (sub (constant 1) (cosE θ1e))))
      v2  = mul m2 (mul g
             (add (mul ℓ1 (sub (constant 1) (cosE θ1e)))
                  (mul ℓ2 (sub (constant 1) (cosE θ2e)))))

  -- Lagrangian L = T1 + T2 - V1 - V2
  return (sub (add t1 t2) (add v1 v2))

main :: IO ()
main = do
  let (coords, lag) = doublePendulum
      raws           = eulerLagrange (coords, lag)
      names          = [ q | Coord q <- coords ]

  putStrLn "Double pendulum equations of motion:"
  forM_ raws $ \(Coord θ, expr) ->
    putStrLn (prettyEL names (θ, expr))
