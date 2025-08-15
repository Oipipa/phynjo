{-# LANGUAGE OverloadedStrings #-}

module Main where

import Phynjo.Core
import Phynjo.Integrators

import qualified Data.Map.Strict as M
import           Physics.Math.Vec3Util
  ( Vec3, vzero, (<+>), (<->), vscale, vdot, vnorm )
import           Physics.Math.LinearAlgebra (cross)

data Scenario = LagrangeTriangle | FigureEight | Sitnikov3D
scenario :: Scenario
scenario = Sitnikov3D

gConst :: Double
gConst = 1.0

dt :: Double
dt = 1.0e-3

nSteps :: Int
nSteps = 40000

masses :: MassMap
masses = case scenario of
  LagrangeTriangle -> M.fromList [(1,1),(2,1),(3,1)]
  FigureEight      -> M.fromList [(1,1),(2,1),(3,1)]
  Sitnikov3D       -> M.fromList [(1,1),(2,1),(3,1.0e-3)]  -- light 3rd mass

initState :: State
initState = case scenario of
  LagrangeTriangle -> cmZero masses lagrangeInit
  FigureEight      -> cmZero masses figureEightInit
  Sitnikov3D       -> cmZero masses sitnikovInit

-- 1) Lagrange equilateral triangle with small out-of-plane wobble
lagrangeInit :: State
lagrangeInit =
  let side      = 1.0
      r         = side / sqrt 3.0
      twoPi120  = 2 * pi / 3
      toPos i   = let th = fromIntegral (i-1) * twoPi120
                  in  ( r * cos th, r * sin th, 0.0 )
      -- Correct ω² = G m / (√3 r³) for equal masses
      omega     = sqrt (gConst / (sqrt 3.0 * r*r*r))
      wobble    = 0.02 * omega
      posMap    = M.fromList [(i, toPos i) | i <- [1..3 :: Int]]
      getVel i  =
        let (x,y,_) = posMap M.! i
            vz      = case i of { 1 ->  wobble; 2 -> -wobble; _ -> 0.0 }
        in  (-omega*y, omega*x, vz)
      velMap    = M.fromList [(i, getVel i) | i <- [1..3 :: Int]]
  in  State { pos = posMap, vel = velMap }

-- 2) Moore–Chenciner figure-eight for three equal masses, G=1
figureEightInit :: State
figureEightInit =
  let q1 = (-0.97000436,  0.24308753, 0.0)
      q2 = ( 0.97000436, -0.24308753, 0.0)
      q3 = ( 0.0       ,  0.0       , 0.0)
      v1 = ( 0.4662036850,  0.4323657300, 0.0)
      v2 = ( 0.4662036850,  0.4323657300, 0.0)
      v3 = (-0.9324073700, -0.8647314600, 0.0)
  in  State { pos = M.fromList [(1,q1),(2,q2),(3,q3)]
            , vel = M.fromList [(1,v1),(2,v2),(3,v3)] }

-- 3) Sitnikov-style: two heavy bodies in a circular binary, light third bouncing in z
sitnikovInit :: State
sitnikovInit =
  let a     = 0.8          -- semi-distance from CM
      m     = 1.0
      -- For two equal masses at separation 2a: ω² = G m / (4 a³)
      omega = sqrt (gConst * m / (4*a*a*a))
      q1 = (-a, 0.0, 0.0)
      q2 = ( a, 0.0, 0.0)
      q3 = ( 0.0, 0.0, 0.4)
      v1 = ( 0.0, -omega*a, 0.0)
      v2 = ( 0.0,  omega*a, 0.0)
      v3 = ( 0.0,  0.0     , 0.0)
  in  State { pos = M.fromList [(1,q1),(2,q2),(3,q3)]
            , vel = M.fromList [(1,v1),(2,v2),(3,v3)] }

-- Center-of-mass recentering and de-drifting, using your Vec3 ops
cmZero :: MassMap -> State -> State
cmZero ms st =
  let mTot = sum (M.elems ms)
      qs   = pos st
      vs   = vel st
      sumWeighted f =
        foldl (\acc (i,mi) -> acc <+> vscale mi (f i)) vzero (M.toList ms)
      rc = vscale (1/mTot) (sumWeighted (\i -> qs M.! i))
      vc = vscale (1/mTot) (sumWeighted (\i -> vs M.! i))
      pos' = M.map (\q -> q <-> rc) qs
      vel' = M.map (\v -> v <-> vc) vs
  in  st { pos = pos', vel = vel' }

-- Diagnostics for CSV
pairwiseDists :: State -> [Double]
pairwiseDists st =
  let q i = pos st M.! i
      d i j = vnorm (q i <-> q j)
  in [d 1 2, d 2 3, d 3 1]

totalAngularMomentum :: MassMap -> State -> Vec3
totalAngularMomentum ms st =
  let term (i,mi) = cross (pos st M.! i) (vscale mi (vel st M.! i))
  in  foldl (<+>) vzero (map term (M.toList ms))

traj :: [State]
traj = integrateN nSteps dt gConst masses initState

header :: String
header = intercalate "," $
     ["t"]
  ++ concat [ ["x"++show i,"y"++show i,"z"++show i
              ,"vx"++show i,"vy"++show i,"vz"++show i]
            | i <- [1..3 :: Int] ]
  ++ ["r12","r23","r31","Lx","Ly","Lz","E"]

row :: Int -> State -> String
row stepIdx st =
  intercalate "," $
    show t : concatMap bodyFields [1..3 :: Int]
      ++ map (printf "%.12g") rs
      ++ map (printf "%.12g") [lx,ly,lz]
      ++ [printf "%.12g" e]
  where
    t  = fromIntegral stepIdx * dt
    qs = pos st
    vs = vel st
    bodyFields i =
      let (x,y,z)    = qs M.! i
          (vx,vy,vz) = vs M.! i
      in map (printf "%.12g") [x,y,z,vx,vy,vz]
    rs         = pairwiseDists st
    (lx,ly,lz) = totalAngularMomentum masses st
    e          = totalEnergy gConst masses st

main :: IO ()
main = do
  putStrLn header
  mapM_ (\(i,s) -> putStrLn (row i s)) (zip [0..] traj)
