-- double pendulum (CAS-only)
{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Ratio ((%))
import           Text.Printf (printf)

import           CAS.AST
  ( Expr(..), (.+.), (.-.), (.*.), (./.), (.^.), neg )
import           CAS.PrettyPrinter (pretty)
import           CAS.Differentiate (differentiate)
import           CAS.Simplify (simplifyFix)
import           Physics.Lagrangian
  ( Coord(..), LagM, defineCoord, timeDeriv, buildLagrangian
  , eulerLagrange, q
  )

-- ------- small helpers -------
c :: Rational -> Expr
c = Const

(^!) :: Expr -> Integer -> Expr
(^!) x k = x .^. Const (fromInteger k)

square :: Expr -> Expr
square x = x .*. x

-- ------- Lagrangian (m1=m2=1, l1=l2=1) -------
doublePendulumL :: ([Coord], Expr)
doublePendulumL = buildLagrangian $ do
  th1  <- defineCoord "theta1"
  th2  <- defineCoord "theta2"
  let θ1 = q th1
      θ2 = q th2
  θ1d <- timeDeriv th1
  θ2d <- timeDeriv th2

  let m1 = c 1
      m2 = c 1
      l1 = c 1
      l2 = c 1
      g  = Var "g"            -- or c (981 % 100) if you want numeric in L

  -- KE of mass 1
  let t1  = c (1 % 2) .*. m1 .*. (l1 ^! 2) .*. square θ1d

  -- velocity of mass 2
  let vx2 = l1 .*. (Cos θ1) .*. θ1d .+. l2 .*. (Cos θ2) .*. θ2d
      vy2 = l1 .*. (Sin θ1) .*. θ1d .+. l2 .*. (Sin θ2) .*. θ2d
      t2  = c (1 % 2) .*. m2 .*. (square vx2 .+. square vy2)

  -- Potential (zero at ceiling): V = m g (l - l cos θ)
  let v1 = m1 .*. g .*. l1 .*. (c 1 .-. Cos θ1)
      v2 = m2 .*. g .*. ( l1 .*. (c 1 .-. Cos θ1)
                      .+. l2 .*. (c 1 .-. Cos θ2) )

  pure $ (t1 .+. t2) .-. (v1 .+. v2)

-- ------- EL residuals and mass-matrix extraction -------
coords@[Coord _, Coord _] = fst doublePendulumL
lag                       = snd doublePendulumL
eqns :: [(Coord, Expr)]
eqns = eulerLagrange (coords, lag)

res1, res2 :: Expr
res1 = snd (eqns !! 0)
res2 = snd (eqns !! 1)

-- coefficients ∂res_i / ∂thetä_j
c11, c12, c21, c22 :: Expr
c11 = simplifyFix $ differentiate "theta1_ddot" res1
c12 = simplifyFix $ differentiate "theta2_ddot" res1
c21 = simplifyFix $ differentiate "theta1_ddot" res2
c22 = simplifyFix $ differentiate "theta2_ddot" res2

-- remainders: r_i = res_i − Σ_j c_ij * thetä_j
c1_expr, c2_expr :: Expr
c1_expr = simplifyFix $ res1 .-. (c11 .*. Var "theta1_ddot" .+. c12 .*. Var "theta2_ddot")
c2_expr = simplifyFix $ res2 .-. (c21 .*. Var "theta1_ddot" .+. c22 .*. Var "theta2_ddot")

-- ------- numeric evaluator -------
evalExpr :: Map String Double -> Expr -> Double
evalExpr env = \case
  Var s     -> M.findWithDefault (err ("unbound var " ++ s)) s env
  Const r   -> fromRational r
  Add a b   -> evalExpr env a + evalExpr env b
  Sub a b   -> evalExpr env a - evalExpr env b
  Mul a b   -> evalExpr env a * evalExpr env b
  Div a b   -> evalExpr env a / evalExpr env b
  Pow a b   -> evalExpr env a ** evalExpr env b
  Neg a     -> negate (evalExpr env a)
  Sin a     -> sin (evalExpr env a)
  Cos a     -> cos (evalExpr env a)
  Tan a     -> tan (evalExpr env a)
  Exp a     -> exp (evalExpr env a)
  Log a     -> log (evalExpr env a)
  where
    err m = error ("evalExpr: " ++ m)

-- ------- dynamics: solve M*a + r = 0 for a -------
type State = (Double,Double,Double,Double)  -- (theta1, omega1, theta2, omega2)

computeAccel :: Double -> State -> (Double,Double)
computeAccel gVal (theta1,omega1,theta2,omega2) =
  let env = M.fromList
        [ ("theta1",     theta1)
        , ("theta2",     theta2)
        , ("theta1_dot", omega1)
        , ("theta2_dot", omega2)
        , ("g",          gVal)
        ]
      [m11,m12,m21,m22] = map (evalExpr env) [c11,c12,c21,c22]
      [r1, r2]          = map (evalExpr env) [c1_expr, c2_expr]
      det = m11*m22 - m12*m21
      a1  = (-m22*r1 + m12*r2) / det
      a2  = ( m21*r1 - m11*r2) / det
  in (a1,a2)

-- ------- RK4 integrator -------
rk4Step :: (State->State) -> Double -> State -> State
rk4Step f h s0 = ( x0 + h*(k11+2*k21+2*k31+k41)/6
                 , v0 + h*(k12+2*k22+2*k32+k42)/6
                 , y0 + h*(k13+2*k23+2*k33+k43)/6
                 , w0 + h*(k14+2*k24+2*k34+k44)/6 )
  where
    (x0,v0,y0,w0)    = s0
    (k11,k12,k13,k14)= f s0
    s1               = (x0+0.5*h*k11, v0+0.5*h*k12, y0+0.5*h*k13, w0+0.5*h*k14)
    (k21,k22,k23,k24)= f s1
    s2               = (x0+0.5*h*k21, v0+0.5*h*k22, y0+0.5*h*k23, w0+0.5*h*k24)
    (k31,k32,k33,k34)= f s2
    s3               = (x0+h*k31, v0+h*k32, y0+h*k33, w0+h*k34)
    (k41,k42,k43,k44)= f s3

-- system derivative
fDyn :: Double -> State -> State
fDyn gVal (theta1,omega1,theta2,omega2) =
  let (a1,a2) = computeAccel gVal (theta1,omega1,theta2,omega2)
  in (omega1, a1, omega2, a2)

-- ------- sim params -------
dt, tmax, gVal :: Double
dt   = 0.01
tmax = 20.0
gVal = 9.81

steps :: Int
steps = floor (tmax / dt)

initial :: State
initial = (1.0, 0.0, 0.5, 0.0)

simulate :: [State]
simulate = take (steps+1) $ iterate (rk4Step (fDyn gVal) dt) initial

-- ------- main -------
main :: IO ()
main = do
  -- optional: print the symbolic EL equations once
  putStrLn "Double pendulum equations of motion:"
  mapM_ (\(Coord name, e) -> putStrLn $ "[" ++ name ++ "]: " ++ pretty e ++ " = 0") eqns

  -- numeric sim CSV
  putStrLn "t,theta1,omega1,theta2,omega2"
  let times = [0, dt .. tmax]
  mapM_ (\(t,(θ1,ω1,θ2,ω2)) ->
            printf "%.4f,%.6f,%.6f,%.6f,%.6f\n" t θ1 ω1 θ2 ω2)
        (zip times simulate)
