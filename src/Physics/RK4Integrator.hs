{-# LANGUAGE RecordWildCards #-}

module Physics.RK4Integrator
  ( rk4Step
  , integrateRK4
  ) where

import Physics.LeapfrogNR    (State(..), MassMap, accOne, vadd, vscale)
import qualified Data.Map.Strict as M

-- | One RK4 step for 3D Newtonian gravity:
--   state derivatives are (q̇ = v, v̇ = acceleration).
rk4Step
  :: Double     -- ^ timestep h
  -> Double     -- ^ gravitational constant G
  -> MassMap    -- ^ body-id → mass (kg)
  -> State      -- ^ current state
  -> State      -- ^ next state
rk4Step h g masses st0 = 
  let
      -- f(s): returns (ṙ, ṗ)
      deriv st@State{..} =
        State
          { pos = vel
          , vel = M.mapWithKey (\i _ -> accOne g masses pos i) vel
          }

      -- s1 + s2 (componentwise vector add)
      addSt a b = State
        { pos = M.unionWith vadd (pos a) (pos b)
        , vel = M.unionWith vadd (vel a) (vel b)
        }

      -- k · s (scale both maps)
      scaleSt k st@State{..} = State
        { pos = M.map (vscale k) pos
        , vel = M.map (vscale k) vel
        }

      -- the four RK4 slopes
      k1 = deriv st0
      k2 = deriv (addSt st0 (scaleSt (h/2) k1))
      k3 = deriv (addSt st0 (scaleSt (h/2) k2))
      k4 = deriv (addSt st0 (scaleSt h     k3))

      -- weighted combination
      incr = scaleSt (h/6)
           $ addSt k1
           $ addSt (scaleSt 2 k2)
                   (addSt (scaleSt 2 k3) k4)
  in addSt st0 incr

-- | Integrate for n steps (returns n+1 states including initial).
integrateRK4
  :: Int        -- ^ number of steps
  -> Double     -- ^ timestep h
  -> Double     -- ^ gravitational constant G
  -> MassMap    -- ^ body-id → mass
  -> State      -- ^ initial state
  -> [State]    -- ^ trajectory of length (n+1)
integrateRK4 n h g masses = take (n+1) . iterate (rk4Step h g masses)
