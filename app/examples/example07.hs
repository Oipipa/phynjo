-- app/examples/PendulumExample.hs
{-# LANGUAGE OverloadedStrings #-}

-- | A complete example using the Lagrangian front‐end to derive
--   and print the equations of motion for a simple pendulum.
module Main where

import Physics.Lagrangian
  ( Coord(..)
  , defineCoord
  , timeDeriv
  , buildLagrangian
  , eulerLagrange
  )
import Physics.Symbolic
  ( Expr
  , var
  , constant
  , add, sub, mul
  , cosE
  , pretty
  )

----------------------------------------------------------------------
-- | Build the pendulum Lagrangian
--     L = T − V
--   where
--     T = ½ m ℓ² θ̇²
--     V = m g ℓ (1 − cos θ)
----------------------------------------------------------------------
pendulum :: ([Coord], Expr)
pendulum = buildLagrangian $ do
  θ  <- defineCoord "θ"
  θd <- timeDeriv θ

  let m = constant 1.0
      ℓ = constant 2.0
      g = constant 9.81

      -- kinetic energy T
      tEnergy =
        mul (constant 0.5)
            (mul m (mul ℓ (mul ℓ (mul θd θd))))

      -- potential energy V
      vEnergy =
        mul m (mul g (mul ℓ (sub (constant 1) (cosE (var "θ")))))

  -- L = T − V
  return (sub tEnergy vEnergy)

----------------------------------------------------------------------
-- | Print each Euler–Lagrange equation in the form
--     d/dt(∂L/∂θ̇) − ∂L/∂θ = 0
----------------------------------------------------------------------
main :: IO ()
main = do
  let (coords, lag) = pendulum
      equations     = eulerLagrange (coords, lag)

  putStrLn "Pendulum equations of motion:"
  mapM_ printEqn equations

 where
  printEqn :: (Coord, Expr) -> IO ()
  printEqn (Coord q, expr) =
    putStrLn $ "  [" ++ q ++ "]: " ++ pretty expr ++ " = 0"
