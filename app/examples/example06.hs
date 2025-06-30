-- 3D 3 body simulation
{-# LANGUAGE RecordWildCards #-}

module Main where

import Text.Printf        (printf)
import Control.Monad      (forM_, when)
import System.Environment (getArgs)

----------------------------------------------------------------
--  3D vector utilities
----------------------------------------------------------------

type Vec3 = (Double,Double,Double)

vadd   :: Vec3 -> Vec3 -> Vec3
vadd   (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

vsub   :: Vec3 -> Vec3 -> Vec3
vsub   (x1,y1,z1) (x2,y2,z2) = (x1-x2, y1-y2, z1-z2)

vscale :: Double -> Vec3 -> Vec3
vscale k (x,y,z)            = (k*x, k*y, k*z)

vdot   :: Vec3 -> Vec3 -> Double
vdot   (x1,y1,z1) (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2

vmag   :: Vec3 -> Double
vmag   v                    = sqrt (vdot v v)

----------------------------------------------------------------
--  State & derivatives
----------------------------------------------------------------

-- Each body’s state is a (position, velocity) pair
type State = [(Vec3, Vec3)]

positions  :: State -> [Vec3]
positions  = map fst

velocities :: State -> [Vec3]
velocities = map snd

-- | Compute Newtonian gravitational accelerations for each body
gravAccel :: Double    -- ^ G
          -> [Double]  -- ^ masses
          -> [Vec3]    -- ^ positions
          -> [Vec3]    -- ^ accelerations
gravAccel g ms ps =
  [ foldr vadd (0,0,0)
      [ let dr = vsub pj pi
            r2 = vdot dr dr + 1e-12
            a  = g * mj / (r2 * sqrt r2)
        in vscale a dr
      | (mj, pj, j) <- zip3 ms ps [0..], j /= i
      ]
  | (pi, i) <- zip ps [0..]
  ]

-- | Given G, masses, and current state, produce (dpos, dvel) for each body
derivative :: Double -> [Double] -> State -> [(Vec3, Vec3)]
derivative g ms st =
  let ps = positions st
      vs = velocities st
      as = gravAccel g ms ps
  in zip vs as

----------------------------------------------------------------
--  Classical RK4 integrator
----------------------------------------------------------------

rk4Step :: Double    -- ^ G
        -> [Double]  -- ^ masses
        -> Double    -- ^ dt
        -> State     -- ^ current state
        -> State     -- ^ next state
rk4Step g ms dt s =
  let k1 = derivative g ms s
      s2 = addState s (scaleDeriv (dt/2) k1)
      k2 = derivative g ms s2
      s3 = addState s (scaleDeriv (dt/2) k2)
      k3 = derivative g ms s3
      s4 = addState s (scaleDeriv dt   k3)
      k4 = derivative g ms s4

      ds1 = zipWith addDeriv (scaleDeriv (1/6) k1)
                             (scaleDeriv (1/3) k2)
      ds2 = zipWith addDeriv (scaleDeriv (1/3) k3)
                             (scaleDeriv (1/6) k4)
      ds  = zipWith addDeriv ds1 ds2

  in addState s (scaleDeriv dt ds)

  where
    -- Add a small derivative increment to the state
    addState :: State -> [(Vec3,Vec3)] -> State
    addState = zipWith (\(r,v) (dr,dv) -> (vadd r dr, vadd v dv))

    -- Scale every (dpos, dvel) by a scalar
    scaleDeriv :: Double -> [(Vec3,Vec3)] -> [(Vec3,Vec3)]
    scaleDeriv k = map (\(dr,dv) -> (vscale k dr, vscale k dv))

    -- Add two derivative lists elementwise
    addDeriv :: (Vec3,Vec3) -> (Vec3,Vec3) -> (Vec3,Vec3)
    addDeriv (dr1,dv1) (dr2,dv2) = (vadd dr1 dr2, vadd dv1 dv2)

----------------------------------------------------------------
--  Total energy (kinetic + potential)
----------------------------------------------------------------

totalEnergy :: Double    -- ^ G
            -> [Double]  -- ^ masses
            -> State     -- ^ current state
            -> Double    -- ^ total energy
totalEnergy g ms st =
  let ps = positions st
      vs = velocities st
      -- kinetic: ½ m v²
      ke = sum [ 0.5 * m * vdot v v
               | (m,v) <- zip ms vs
               ]
      -- potential: −G m_i m_j / r_ij  for i<j
      pe = sum
        [ - g * mi * mj / (vmag (vsub pj pi) + 1e-12)
        | ((pi,mi), i) <- zip (zip ps ms) [0..]
        , ((pj,mj), j) <- zip (zip ps ms) [0..]
        , i < j
        ]
  in ke + pe

----------------------------------------------------------------
--  CSV loop
----------------------------------------------------------------

loopCSV
  :: Int       -- ^ remaining steps
  -> Int       -- ^ total steps
  -> Double    -- ^ dt
  -> [Double]  -- ^ masses
  -> State     -- ^ current state
  -> IO ()
loopCSV 0 _ _ _ _ = return ()
loopCSV k nSteps dt masses st = do
  let t  = fromIntegral (nSteps - k) * dt
      ps = positions st
      -- assume exactly three bodies:
      [p1,p2,p3] = ps
      te = totalEnergy g masses st
  -- emit one CSV row, quoting the tuples so their commas don't break columns
  printf "%.6f,\"%s\",\"%s\",\"%s\",%.12f\n"
    t
    (show p1)
    (show p2)
    (show p3)
    te

  loopCSV (k-1) nSteps dt masses (rk4Step g masses dt st)
  where
    g = 1.0

----------------------------------------------------------------
--  main: header + run
----------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let (nSteps, dt) = case args of
        [ns, dt'] -> (read ns, read dt')
        _         -> (5000, 1e-3)

      masses    = [1.0, 0.001, 0.000003]
      initState =
        [ ((0,   0,0), (0,   0,0))  -- body 0
        , ((1,   0,0), (0,   1,0))  -- body 1
        , ((1.1, 0,0), (0,  -1,0))  -- body 2
        ]

  -- print CSV header
  putStrLn "t,p1,p2,p3,TE"
  loopCSV nSteps nSteps dt masses initState
