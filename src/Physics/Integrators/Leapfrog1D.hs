{-# LANGUAGE NamedFieldPuns #-}
module Physics.Integrators.Leapfrog1D
  ( leapfrog1D 
  ) where

import           Base.Components         (Component)
import qualified Data.Set           as S

import           Physics.DriftNR    (driftNR)
import           Physics.Forces.ForceNR    (forceNR)
import           Physics.Forces.Force      (Force)
import           Base.NumericRule        (NumericRule(..))
import           Base.NumericWorkflow    (NumericWorkflow(..))
import           Base.NState             (NState)

leapfrog1D
  :: Double 
  -> [(Component,Double)] 
  -> Force 
  -> NumericWorkflow
leapfrog1D dt masses f =
  let half      = dt / 2
      domain    = S.fromList (map fst masses)

      baseDrift = driftNR masses
      baseKick  = forceNR  f      masses

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
