{-# LANGUAGE TupleSections #-}
module Physics.Integrators.LeapfrogNR
  ( Vec3
  , State(..)
  , MassMap
  , vadd
  , vsub
  , vscale
  , vdot
  , vnorm2
  , accOne
  , drift
  , kick
  , leapRaw
  , adaptive
  , leapfrogNR
  , integrateN
  , totalEnergy
  ) where

import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

type Vec3 = (Double,Double,Double)

vadd, vsub          :: Vec3 -> Vec3 -> Vec3
vscale              :: Double -> Vec3 -> Vec3
vdot                :: Vec3 -> Vec3 -> Double
vnorm2              :: Vec3 -> Double

vadd (a,b,c) (x,y,z) = (a+x , b+y , c+z)
vsub (a,b,c) (x,y,z) = (a-x , b-y , c-z)
vscale k (x,y,z)     = (k*x , k*y , k*z)
vdot  (a,b,c) (x,y,z)=  a*x + b*y + c*z
vnorm2 v             = vdot v v

type MassMap = M.Map Int Double    -- ^ body-id → mass

soft, safety, hMin :: Double
soft   = 1e-3        -- Plummer softening
safety = 20.0        -- CFL parameter (larger ⇒ stricter)
hMin   = 1.0e-15     -- absolute lower bound for dt

data State = State
  { pos :: M.Map Int Vec3          -- ^ positions, rᵢ
  , vel :: M.Map Int Vec3          -- ^ velocities, vᵢ
  } deriving (Eq,Show)

drift :: Double -> State -> State
drift h s =
  s{ pos = M.intersectionWith (\r v -> vadd r (vscale h v))
                              (pos s) (vel s) }

-- | Acceleration on body i from all other bodies.
accOne :: Double              -- ^ G
       -> MassMap
       -> M.Map Int Vec3      -- ^ positions
       -> Int                 -- ^ index i
       -> Vec3
accOne g m rs i =
  let ri = rs M.! i
      force acc j rj
        | i == j   = acc
        | otherwise =
            let dr   = vsub rj ri
                r2   = vnorm2 dr + soft*soft
            in  vadd acc (vscale (g * (m M.! j) / (r2 * sqrt r2)) dr)
  in  M.foldlWithKey' force (0,0,0) rs

-- | Kick velocities for a time h with freshly computed accelerations.
kick :: Double -> Double -> MassMap -> State -> State
kick h g m s =
  let rs  = pos s
      vs  = vel s
      a   = M.mapWithKey (\i _ -> accOne g m rs i) rs
      dv  = M.map (vscale h) a
      -- strict key match; fail fast if something is missing
      vel' = M.mapWithKey
                (\k vOld ->
                   vadd vOld (fromMaybe (err k) (M.lookup k dv)))
                vs
      err k = error ("kick: missing dv for body id " ++ show k)
  in  s{ vel = vel' }

-- | Second-order, time-reversible velocity-Verlet (a.k.a. leap-frog).
leapRaw :: Double -> Double -> MassMap -> State -> State
leapRaw h g m = drift (0.5*h) . kick h g m . drift (0.5*h)

----------------------------------------------------------------------
--  Adaptivity
----------------------------------------------------------------------

-- | Quick CFL-style acceptance test for dt = h.
stepOK :: Double -> MassMap -> State -> Bool
stepOK h m s =
  let ks     = M.keys m
      r2min  = minimum [ vnorm2 (vsub (pos s M.! j) (pos s M.! i))
                       | i<-ks, j<-ks, i<j ] + soft*soft
      vmax2  = maximum [ vnorm2 (vsub (vel s M.! i) (vel s M.! j))
                       | i<-ks, j<-ks, i<j ] + 1e-24
      vmax   = sqrt vmax2
  in  h * safety <= sqrt r2min / vmax

-- | Recursive adaptive wrapper around `leapRaw`.
--   *If h falls below hMin we stop subdividing and accept the step.*
adaptive :: Double -> Double -> MassMap -> State -> State
adaptive h g m s
  | h < hMin     = leapRaw h g m s           -- give up further subdivision
  | stepOK h m s = leapRaw h g m s           -- accepted first try
  | otherwise    = let h2 = 0.5 * h
                       s' = adaptive h2 g m s
                   in  adaptive h2 g m s'

-- | Adaptive velocity-Verlet step.
leapfrogNR :: Double -> Double -> MassMap -> State -> State
leapfrogNR = adaptive

-- | Integrate for n steps, returning the whole trajectory.
integrateN :: Int -> Double -> Double -> MassMap -> State -> [State]
integrateN n h g m = take (n + 1) . iterate (leapfrogNR h g m)

-- | Total (kinetic + potential) energy.
totalEnergy :: Double -> MassMap -> State -> Double
totalEnergy g m s =
  let ke = sum [ 0.5 * mi * vnorm2 (vel s M.! i)
               | (i,mi) <- M.toList m ]
      ids = M.keys m
      pe  = sum [ let dr = vsub (pos s M.! j) (pos s M.! i)
                      r  = sqrt (vnorm2 dr + soft*soft)
                  in  -g * (m M.! i) * (m M.! j) / r
                | i<-ids, j<-ids, i<j ]
  in  ke + pe
