module Physics.Integrators.EulerNR
  ( eulerNR
  ) where

import Components             (Component)
import Physics.Forces.GravNR         (gravNR)
import Physics.DriftNR        (driftNR)
import NumericWorkflow        (NumericWorkflow(..))

eulerNR
  :: Double 
  -> Double 
  -> [(Component,Double)]
  -> NumericWorkflow
eulerNR dt g masses =
  let kick  = Run (gravNR dt g masses)
      drift = Run (driftNR masses)
  in Seq kick drift
