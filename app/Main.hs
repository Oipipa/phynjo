module Main where

import qualified Data.Map.Strict    as M
import           Physics.LeapfrogNR

-- Physical constants & run parameters ----------------------------------------

gConst :: Double
gConst = 1.0

dt :: Double
dt = 1e-3                          -- small enough for stability

nSteps :: Int
nSteps = 200000                    -- final time = 200 s

-- Masses and initial conditions ----------------------------------------------

masses :: MassMap
masses = M.fromList [(1,1.0), (2,1.0), (3,1.0)]

initState :: State
initState =
  State { pos = M.fromList [(1,(-1,0,0)), (2,(0,0,0)), (3,(1,0,0))]
        , vel = M.fromList [(1,(0,0,0)),  (2,(0,0,0)), (3,(0,0,0))] }

-- Trajectory ------------------------------------------------------------------

traj :: [State]
traj = integrateN nSteps dt gConst masses initState

-- CSV helpers -----------------------------------------------------------------

vecX :: Vec3 -> Double
vecX (x,_,_) = x

lookupX, lookupV :: Int -> State -> Double
lookupX i s = vecX (pos s M.! i)
lookupV i s = vecX (vel s M.! i)

row :: Int -> State -> String
row k s =
  let t  = fromIntegral k * dt
      q1 = lookupX 1 s; q2 = lookupX 2 s; q3 = lookupX 3 s
      p1 = lookupV 1 s; p2 = lookupV 2 s; p3 = lookupV 3 s
      e  = totalEnergy gConst masses s
  in concatMap (++ ",")
       [ show t, show q1, show q2, show q3
       , show p1, show p2, show p3 ] ++ show e

main :: IO ()
main = do
  putStrLn "t,q1,q2,q3,p1,p2,p3,E"
  mapM_ (uncurry rowPrint) (zip [0..] traj)
 where
  rowPrint i s = putStrLn (row i s)
