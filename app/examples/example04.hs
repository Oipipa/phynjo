{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map.Strict       as M
import           Physics.LeapfrogNR    (State(..), MassMap, integrateN, totalEnergy)
import           Physics.LeapfrogNR    (Vec3)
import Numeric.Units.Dimensional.Prelude    ( (*~), kilo, gram )
import qualified Numeric.Units.Dimensional.Prelude as D

-- gravitational constant, time‐step and steps
gConst :: Double
gConst = 6.67430e-11

dt :: Double
dt = 60      -- 1 minute

nSteps :: Int
nSteps = 24*60  -- simulate 1 day

-- IDs
sunId, planetId :: Int
sunId    = 1
planetId = 2

-- Masses (kg)
massMap :: MassMap
massMap = M.fromList
  [ (sunId,    1.0e30)    -- central mass
  , (planetId, 1.0e24)    -- small “planet”
  ]

-- Initial State: circular orbit in XY plane at r = 1e9 m
initState :: State
initState = State
  { pos = M.fromList
      [ (sunId,    (0,0,0))
      , (planetId, (r,0,0))
      ]
  , vel = M.fromList
      [ (sunId,    (0,0,0))
      -- v = sqrt(GM/r) in y direction
      , (planetId, (0, v, 0))
      ]
  }
  where
    r = 1e9
    v = sqrt (gConst * (massMap M.! sunId) / r)

-- Integrate
trajectory :: [State]
trajectory = integrateN nSteps dt gConst massMap initState

-- CSV output: t, x, y
printRow :: Int -> State -> IO ()
printRow i st =
  let t          = fromIntegral i * dt
      (x, y, _)  = pos st M.! planetId
      energy     = totalEnergy gConst massMap st
  in  putStrLn $ show t ++ "," ++ show x ++ "," ++ show y ++ "," ++ show energy

main :: IO ()
main = do
  putStrLn "t,x,y,E"
  mapM_ (uncurry printRow) (zip [0..] trajectory)
