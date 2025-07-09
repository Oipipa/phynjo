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

pendulum :: ([Coord], Expr)
pendulum = buildLagrangian $ do
  theta  <- defineCoord "theta"
  thetad <- timeDeriv theta

  let m = constant 1.0
      l = constant 2.0
      g = constant 9.81

      tEnergy =
        constant 0.5 `mul` m
                       `mul` l `mul` l
                       `mul` thetad `mul` thetad

      vEnergy =
        m `mul` g `mul` l
          `mul` (sub (constant 1.0) (cosE (var "theta")))

  return (sub tEnergy vEnergy)

main :: IO ()
main = do
  let (coords, lagrangian) = pendulum
      rawEqns              = eulerLagrange (coords, lagrangian)

      names = [ q | Coord q <- coords ]
      eqns  = [ (q, expr) | (Coord q, expr) <- rawEqns ]

  putStrLn "Pendulum equations of motion (factored & simplified):"
  mapM_ (putStrLn . prettyEL names) eqns
