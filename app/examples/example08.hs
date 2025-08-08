{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad          (forM_)
import           Data.Ratio             ((%))

import           CAS.AST
  ( Expr(..), (.+.), (.-.), (.*.), (./.), (.^.), neg
  )
import           CAS.PrettyPrinter      (pretty)
import           Physics.Lagrangian
  ( Coord(..)
  , LagM, defineCoord, timeDeriv, buildLagrangian
  , eulerLagrange, q
  )

-- small helpers
c :: Rational -> Expr
c = Const
(^!) :: Expr -> Integer -> Expr
(^!) x k = x .^. Const (fromInteger k)  -- integer power

square :: Expr -> Expr
square x = x .*. x

-- Build the Lagrangian of a planar double pendulum (m1=m2=1, l1=l2=1).
-- Keep g symbolic (Var "g") or set it exactly with c (981 % 100).
doublePendulum :: ([Coord], Expr)
doublePendulum = buildLagrangian $ do
  theta1 <- defineCoord "theta1"
  theta2 <- defineCoord "theta2"

  let th1 = q theta1
      th2 = q theta2

  th1d <- timeDeriv theta1
  th2d <- timeDeriv theta2

  -- params (exact rationals)
  let m1 = c 1
      m2 = c 1
      l1 = c 1
      l2 = c 1
      g  = Var "g"          -- or: c (981 % 100)

  -- KE of mass 1: (1/2) m1 l1^2 * th1d^2
  let t1  = c (1 % 2) .*. m1 .*. (l1 ^! 2) .*. square th1d

  -- velocities of mass 2
  let vx2 = l1 .*. (Cos th1) .*. th1d .+. l2 .*. (Cos th2) .*. th2d
      vy2 = l1 .*. (Sin th1) .*. th1d .+. l2 .*. (Sin th2) .*. th2d

  -- KE of mass 2: (1/2) m2 (vx2^2 + vy2^2)
  let t2  = c (1 % 2) .*. m2 .*. (square vx2 .+. square vy2)

  -- Potential energies: choose zero at ceiling so V = m g (l - l cos θ)
  let v1  = m1 .*. g .*. l1 .*. (c 1 .-. Cos th1)
      v2  = m2 .*. g .*. ( l1 .*. (c 1 .-. Cos th1)
                        .+. l2 .*. (c 1 .-. Cos th2) )

  -- L = T1 + T2 - (V1 + V2)
  pure $ (t1 .+. t2) .-. (v1 .+. v2)

main :: IO ()
main = do
  let (coords, lag) = doublePendulum
      eqs           = eulerLagrange (coords, lag)
  putStrLn "Double pendulum equations of motion:"
  forM_ eqs $ \(Coord name, expr) ->
    putStrLn $ "[" ++ name ++ "]: " ++ pretty expr ++ " = 0"
