-- app/examples/PendulumPretty.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Physics.Lagrangian
  ( Coord(..)
  , defineCoord
  , timeDeriv
  , buildLagrangian
  , eulerLagrange
  )
import SymbolicPhysics.SymbolicD
  ( Expr
  , var
  , constant
  , add, sub, mul
  , cosE
  )
import SymbolicPhysics.PrettyEL
  ( prettyEL
  )

-- | Simple pendulum (m=1, ℓ=2, g=9.81)
pendulum :: ([Coord], Expr)
pendulum = buildLagrangian $ do
  θ  <- defineCoord "θ"
  θd <- timeDeriv θ

  let m = constant 1.0
      ℓ = constant 2.0
      g = constant 9.81

      -- T = ½ m ℓ² θ̇²
      tEnergy =
        constant 0.5 `mul` m
                       `mul` ℓ `mul` ℓ
                       `mul` θd `mul` θd

      -- V = m g ℓ (1 − cos θ)
      vEnergy =
        m `mul` g `mul` ℓ
          `mul` (sub (constant 1.0) (cosE (var "θ")))

  return (sub tEnergy vEnergy)

main :: IO ()
main = do
  let (coords, lagrangian) = pendulum
      rawEqns              = eulerLagrange (coords, lagrangian)

      -- names = ["θ"]
      names = [ q | Coord q <- coords ]
      eqns  = [ (q, expr) | (Coord q, expr) <- rawEqns ]

  putStrLn "Pendulum equations of motion (factored & simplified):"
  mapM_ (putStrLn . prettyEL names) eqns
