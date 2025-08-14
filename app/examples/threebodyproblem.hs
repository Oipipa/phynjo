{-# LANGUAGE OverloadedStrings #-}

module Main where

import Phynjo.Core 
import Phynjo.Integrators 

import qualified Data.Map.Strict as M

gConst, dt :: Double
gConst = 1.0
dt     = 1e-3

nSteps :: Int
nSteps = 20000

masses :: MassMap
masses = M.fromList
  [ (1, 1.0)
  , (2, 1.0)
  , (3, 1.0)
  ]

side :: Double
side = 1.0

sqrt3, r :: Double
sqrt3 = sqrt 3
r     = side / sqrt3

omega :: Double
omega = sqrt (gConst * 3.0 / (r**3))

vZamplitude :: Double
vZamplitude = 0.1 * omega

initState :: State
initState = State
  { pos = posMap
  , vel = velMap
  }
  where
    twoPi120 = 2 * pi / 3
    toPos i  = let theta = fromIntegral (i-1) * twoPi120
               in ( r * cos theta
                  , r * sin theta
                  , 0.0
                  )
    posMap = M.fromList [ (i, toPos i) | i <- [1..3 :: Int] ]

    velMap = M.fromList
      [ (1, let (x,y,_) = posMap M.! 1
             in (-omega*y, omega*x,  vZamplitude) )
      , (2, let (x,y,_) = posMap M.! 2
             in (-omega*y, omega*x, -vZamplitude) )
      , (3, let (x,y,_) = posMap M.! 3
             in (-omega*y, omega*x,  0.0         ) )
      ]

traj :: [State]
traj = integrateN nSteps dt gConst masses initState

header :: String
header = intercalate "," $
     ["t"]
  ++ concat [ ["x"++show i, "y"++show i, "z"++show i
              , "vx"++show i,"vy"++show i,"vz"++show i]
            | i <- [1..3 :: Int] ]
  ++ ["E"]

row :: Int -> State -> String
row stepIdx st =
  intercalate "," $
    show t : concatMap bodyFields [1..3 :: Int] ++ [printf "%.12g" e]
  where
    t  = fromIntegral stepIdx * dt
    qs = pos st
    vs = vel st
    bodyFields i =
      let (x,y,z)    = qs M.! i
          (vx,vy,vz) = vs M.! i
      in map (printf "%.12g") [x,y,z,vx,vy,vz]
    e = totalEnergy gConst masses st

main :: IO ()
main = do
  putStrLn header
  mapM_ (\(i,s) -> putStrLn (row i s)) (zip [0..] traj)
