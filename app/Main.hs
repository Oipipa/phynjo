{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map.Strict as M
import           Physics.LeapfrogNR

-- | Physical constants & run parameters
gConst :: Double
gConst = 1.0

dt :: Double
dt = 1e-3               -- time step

nSteps :: Int
nSteps = 20000          -- run for t = nSteps*dt = 20 time-units

-- | Two masses: a heavy “sun” (id 1) fixed at the origin, 
--   and a light planet (id 2) in a circular orbit.
masses :: MassMap
masses = M.fromList
  [ (1, 100.0)  -- central mass
  , (2,   1.0)  -- orbiting mass
  ]

-- | Initial state: sun at rest at origin, planet at x=1 with vy = sqrt(G·M/r)
initState :: State
initState = State
  { pos = M.fromList
      [ (1, (0,0,0))
      , (2, (1,0,0))
      ]
  , vel = M.fromList
      [ (1, (0,0,0))
      , (2, (0, v0, 0))
      ]
  }
  where
    v0 = sqrt (gConst * (masses M.! 1) / 1)  -- r = 1

-- | Build the trajectory
traj :: [State]
traj = integrateN nSteps dt gConst masses initState

-- | Helpers to extract x, y and energy
row :: Int -> State -> String
row i s =
  let t  = fromIntegral i * dt
      (x1,_,_) = pos s M.! 1
      (x2,y2,_) = pos s M.! 2
      (vx2,vy2,_) = vel s M.! 2
      e  = totalEnergy gConst masses s
  in unwordsWithComma
       [ show t
       , show x2, show y2
       , show vx2, show vy2
       , show e
       ]

unwordsWithComma :: [String] -> String
unwordsWithComma = foldr1 (\a b -> a ++ ',' : b)

-- | Main: print CSV to stdout
main :: IO ()
main = do
  putStrLn "t,x,y,vx,vy,E"
  mapM_ (uncurry rowPrint) (zip [0..] traj)
 where
  rowPrint i s = putStrLn (row i s)
