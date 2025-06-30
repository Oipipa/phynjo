-- 3D 3-body problem
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Physics.LeapfrogNR
  ( State(..)
  , MassMap
  , integrateN
  , totalEnergy
  )
import qualified Data.Map.Strict as M
import Control.Monad   (forM_)
import Text.Printf     (printf)

-- | Gravitational constant
gConst :: Double
gConst = 1.0

-- | Time step and number of steps
dt     :: Double
dt     = 1e-3

nSteps :: Int
nSteps = 20000

-- | Three equal masses
masses :: MassMap
masses = M.fromList
  [ (1, 1.0)
  , (2, 1.0)
  , (3, 1.0)
  ]

-- | Build the initial state: bodies on an equilateral triangle of side √3
--   (so radius=1), rotating rigidly about z.
initState :: State
initState =
  let root3 = sqrt 3
      -- positions at 0°, 120°, 240° around a unit circle
      posMap = M.fromList
        [ (1, ( 1.0     , 0.0       , 0.0 ))
        , (2, ( -0.5     ,  root3/2 , 0.0 ))
        , (3, ( -0.5     , -root3/2 , 0.0 ))
        ]
      -- angular freq for rigid rotation: ω = √(G·M_total/R³)
      ω     = sqrt (gConst * 3.0 / (1.0**3))
      -- speed = ω·R = ω*1
      v     = ω
      -- velocities perpendicular to radius vectors
      velMap = M.map
        (\(x,y,z) -> (-y*v, x*v, 0.0))
        posMap
  in State { pos = posMap, vel = velMap }

-- | Trajectory: integrate for nSteps
traj :: [State]
traj = integrateN nSteps dt gConst masses initState

-- | Print CSV line: t, x1,y1,E, x2,y2,E, x3,y3,E
printLine :: Int -> State -> IO ()
printLine i st = do
  let t  = fromIntegral i * dt
      (x1,y1,_) = pos st M.! 1
      (x2,y2,_) = pos st M.! 2
      (x3,y3,_) = pos st M.! 3
      e         = totalEnergy gConst masses st
  printf "%.4f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f\n"
    t
    x1 y1 e
    x2 y2 e
    x3 y3 e

main :: IO ()
main = do
  putStrLn "t,x1,y1,E,x2,y2,E,x3,y3,E"
  forM_ (zip [0..] traj) (uncurry printLine)
