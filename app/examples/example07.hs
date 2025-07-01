-- Pendulum 
{-# LANGUAGE OverloadedStrings #-}

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

  return (sub tEnergy vEnergy)

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
