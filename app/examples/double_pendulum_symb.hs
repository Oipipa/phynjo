{-# LANGUAGE OverloadedStrings #-}

module Main where

import Phynjo.SymbolicPhysics
import Phynjo.Core

-- small helpers
c :: Rational -> Expr
c = Const
(^!) :: Expr -> Integer -> Expr
(^!) x k = x .^. Const (fromInteger k)  -- integer power

square :: Expr -> Expr
square x = x .*. x

doublePendulum :: ([Coord], Expr)
doublePendulum = buildLagrangian $ do
  theta1 <- defineCoord "theta1"
  theta2 <- defineCoord "theta2"

  let th1 = qlag theta1
      th2 = qlag theta2

  th1d <- timeDeriv theta1
  th2d <- timeDeriv theta2

  -- params (exact rationals)
  let m1 = c 1
      m2 = c 1
      l1 = c 1
      l2 = c 1
      g  = Var "g"          -- or: c (981 % 100)

  let t1  = c (1 % 2) .*. m1 .*. (l1 ^! 2) .*. square th1d

  let vx2 = l1 .*. (Cos th1) .*. th1d .+. l2 .*. (Cos th2) .*. th2d
      vy2 = l1 .*. (Sin th1) .*. th1d .+. l2 .*. (Sin th2) .*. th2d

  let t2  = c (1 % 2) .*. m2 .*. (square vx2 .+. square vy2)

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
