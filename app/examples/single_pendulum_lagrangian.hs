{-# LANGUAGE OverloadedStrings #-}

module Main where

import Phynjo.SymbolicPhysics
import Phynjo.Core

-- tiny helpers
c :: Rational -> Expr
c = Const

(^!) :: Expr -> Integer -> Expr
(^!) x k = x .^. Const (fromInteger k)

square :: Expr -> Expr
square x = x .*. x

-- Pendulum L(qlag, q̇) with m=1, l=2, g symbolic (or set g = c (981 % 100))
pendulum :: ([Coord], Expr)
pendulum = buildLagrangian $ do
  theta  <- defineCoord "theta"
  thetad <- timeDeriv theta

  let m = c 1           -- 1 kg
      l = c 2           -- 2 m
      g = Var "g"       -- keep symbolic; or: c (981 % 100)

      tEnergy = c (1 % 2) .*. m .*. (l ^! 2) .*. square thetad
      vEnergy = m .*. g .*. l .*. (c 1 .-. Cos (qlag theta))

  pure (tEnergy .-. vEnergy)

main :: IO ()
main = do
  let (coords, lagrangian) = pendulum
      eqns                 = eulerLagrange (coords, lagrangian)

  putStrLn "Pendulum equations of motion (simplified):"
  forM_ eqns $ \(Coord name, expr) ->
    putStrLn $ "[" ++ name ++ "]: " ++ pretty expr ++ " = 0"
