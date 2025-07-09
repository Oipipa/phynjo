{-# LANGUAGE NamedFieldPuns #-}
module Physics.Integrators.Symplectic4
  ( symplectic4    -- :: Double -> [(Component,Double)] -> Force -> NumericWorkflow
  ) where

import           Components           (Component)
import qualified Data.Set             as S

import           Physics.DriftNR      (driftNR)
import           Physics.Forces.ForceNR      (forceNR)
import           Physics.Forces.Force        (Force)
import           NumericRule          (NumericRule(..))
import           NumericWorkflow      (NumericWorkflow(..))

symplectic4
  :: Double 
  -> [(Component,Double)] 
  -> Force 
  -> NumericWorkflow
symplectic4 dt masses f =
  let 
      θ  = 1 / (2 - 2 ** (1/3))
      c1 = θ / 2
      c2 = (1 - θ) / 2
      d1 = θ
      d2 = 1 - 2*θ

      -- chop out base rules
      baseDrift = driftNR masses
      baseKick  = forceNR  f masses

      -- full set of bodies
      domain = S.fromList (map fst masses)

      mkSlice (ruleF, alpha) =
        let nr = NumericRule
                   { nrDomain = domain
                   , nrStep   = \_ st -> nrStep ruleF (alpha * dt) st
                   }
        in Run nr

      -- the sequence of (rule, fraction) pairs
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
