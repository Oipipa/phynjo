{-# LANGUAGE NamedFieldPuns #-}
module Physics.Integrators.Symplectic4
  ( symplectic4 
  ) where

import           Base.Components           (Component)
import qualified Data.Set             as S

import           Physics.DriftNR      (driftNR)
import           Physics.Forces.ForceNR      (forceNR)
import           Physics.Forces.Force        (Force)
import           Base.NumericRule          (NumericRule(..))
import           Base.NumericWorkflow      (NumericWorkflow(..))

symplectic4
  :: Double 
  -> [(Component,Double)] 
  -> Force 
  -> NumericWorkflow
symplectic4 dt masses f =
  let 
      theta  = 1 / (2 - 2 ** (1/3))
      c1 = theta / 2
      c2 = (1 - theta) / 2
      d1 = theta
      d2 = 1 - 2*theta

      baseDrift = driftNR masses
      baseKick  = forceNR  f masses

      domain = S.fromList (map fst masses)

      mkSlice (ruleF, alpha) =
        let nr = NumericRule
                   { nrDomain = domain
                   , nrStep   = \_ st -> nrStep ruleF (alpha * dt) st
                   }
        in Run nr

      schedule =
        [ (baseDrift, c1)
        , (baseKick , d1)
        , (baseDrift, c2)
        , (baseKick , d2)
        , (baseDrift, c2)
        , (baseKick , d1)
        , (baseDrift, c1)
        ]
  in foldr1 Seq (map mkSlice schedule)
