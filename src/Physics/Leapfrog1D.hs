{-# LANGUAGE NamedFieldPuns #-}
module Physics.Leapfrog1D
  ( leapfrog1D  -- :: Double -> [(Component,Double)] -> Force -> NumericWorkflow
  ) where

import           Components         (Component)
import qualified Data.Set           as S

import           Physics.DriftNR    (driftNR)
import           Physics.ForceNR    (forceNR)
import           Physics.Force      (Force)
import           NumericRule        (NumericRule(..))
import           NumericWorkflow    (NumericWorkflow(..))
import           NState             (NState)

leapfrog1D
  :: Double                -- ^ full time‐step Δt
  -> [(Component,Double)]  -- ^ bodies with masses
  -> Force                 -- ^ force field
  -> NumericWorkflow
leapfrog1D dt masses f =
  let half      = dt / 2
      domain    = S.fromList (map fst masses)

      -- the underlying drift and kick rules
      baseDrift = driftNR masses
      baseKick  = forceNR  f      masses

      -- wrap them so they ignore the workflow‐dt and use our chosen half/full steps
      driftHalfRule = NumericRule
        { nrDomain = domain
        , nrStep   = \_ st -> nrStep baseDrift half st
        }

      kickFullRule = NumericRule
        { nrDomain = domain
        , nrStep   = \_ st -> nrStep baseKick dt st
        }

  in  Seq (Run driftHalfRule)
          (Seq (Run kickFullRule)
               (Run driftHalfRule))
