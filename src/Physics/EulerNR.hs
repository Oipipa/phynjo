module Physics.EulerNR
  ( eulerNR
  ) where

import Components             (Component)
import Physics.GravNR         (gravNR)
import Physics.DriftNR        (driftNR)
import NumericWorkflow        (NumericWorkflow(..))

-- | First-order (“kick → drift”) Euler integrator.
--
--   * kick:   update momenta via Newtonian gravity
--   * drift:  update positions via p/m
--
-- Δt is the time step, G is the gravitational constant.
eulerNR
  :: Double                   -- ^ Δt
  -> Double                   -- ^ G
  -> [(Component,Double)]     -- ^ masses
  -> NumericWorkflow
eulerNR dt g masses =
  let kick  = Run (gravNR dt g masses)
      drift = Run (driftNR masses)
  in Seq kick drift
