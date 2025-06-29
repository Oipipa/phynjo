module Physics.EulerNR
  ( eulerNR            -- :: dt -> G -> [(Component,Double)] -> NSpell
  ) where

import Components        (Component)
import NSpell            (NSpell (..))
import Physics.GravNR    (gravNR)
import Physics.DriftNR   (driftNR)

-- | Classic first-order ‘kick → drift’ Euler step.
--
--   • gravNR uses the same dt to update momenta
--   • driftNR uses dt (passed later by the spell runner) for positions
eulerNR
  :: Double                 -- ^ Δt
  -> Double                 -- ^ G (gravitational constant)
  -> [(Component,Double)]   -- ^ masses (positive)
  -> NSpell
eulerNR dt g masses =
  let kick  = NRun (gravNR dt g masses)
      drift = NRun (driftNR masses)
  in  NSeq kick drift
