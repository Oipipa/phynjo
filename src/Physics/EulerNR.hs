module Physics.EulerNR
  ( eulerNR
  ) where

import Components             (Component)
import Physics.GravNR         (gravNR)
import Physics.DriftNR        (driftNR)
import NumericWorkflow        (NumericWorkflow(..))

eulerNR
  :: Double                   -- ^ Δt
  -> Double                   -- ^ G
  -> [(Component,Double)]     -- ^ masses
  -> NumericWorkflow
eulerNR dt g masses =
  let kick  = Run (gravNR dt g masses)
      drift = Run (driftNR masses)
  in Seq kick drift
