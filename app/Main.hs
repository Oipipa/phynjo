{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Physics.LeapfrogNR    (State(..), MassMap, integrateN, totalEnergy)
import qualified Data.Map.Strict       as M

gConst :: Double
gConst = 1.0

dt :: Double
dt = 1e-3

nSteps :: Int
nSteps = 20000  

masses :: MassMap
masses = M.fromList
  [ (1, 100.0)   -- “Sun”
  , (2,   1.0)   -- “Planet”
  ]

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
    v0 = sqrt (gConst * (masses M.! 1) / 1.0)

traj :: [State]
traj = integrateN nSteps dt gConst masses initState

xOf :: State -> Double
xOf st = let (x,_,_) = pos st M.! 2 in x

-- extract x‐momentum of body 2
pOf :: State -> Double
pOf st = let (px,_,_) = vel st M.! 2 in px

-- relative error helper
relErr :: Double -> Double -> Double
relErr ref actual = (actual - ref) / abs ref

uncomma :: [String] -> String
uncomma = foldr1 (\a b -> a ++ ',' : b)

row :: Int -> State -> String
row i st =
  let t   = fromIntegral i * dt

      -- raw values
      x   = xOf st
      p   = pOf st
      e   = totalEnergy gConst masses st

      -- reference (initial) values
      x0  = xOf initState
      p0  = pOf initState
      e0  = totalEnergy gConst masses initState

      -- relative errors
      dx  = relErr x0 x
      dp  = if p0 == 0 then 0 else relErr p0 p
      de  = relErr e0 e

  in uncomma
      [ show t
      , show x
      , show dx
      , show p
      , show dp
      , show e
      , show de
      ]

main :: IO ()
main = do
  putStrLn "t,x,Δx/x0,px,Δpx/px0,E,ΔE/E0"
  mapM_ (uncurry (\i s -> putStrLn (row i s))) (zip [0..] traj)
