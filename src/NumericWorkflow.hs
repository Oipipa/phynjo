{-# LANGUAGE NamedFieldPuns #-}

-- | Composition of ‘NumericRule’s over the numeric state (‘NState’).
--   Supports sequential and parallel composition; parallel requires disjoint domains.
module NumericWorkflow
  ( NumericWorkflow(..)
  , workflowDomain
  , applyNumericWorkflow
  ) where

import           Components         (Component)
import qualified Data.Set           as Set
import qualified Data.Map.Strict    as M

import           NumericRule        (NumericRule(..), applyNumericRule)
import           NState             (NState(..))
import           ScalarLiteral      (SLit(..))  -- for mergeSL

----------------------------------------------------------------------

-- | A numeric workflow: either a single rule, or sequential / parallel composition.
data NumericWorkflow
  = Run NumericRule
  | Seq NumericWorkflow NumericWorkflow
  | Par NumericWorkflow NumericWorkflow

-- | Which components this workflow may touch
workflowDomain :: NumericWorkflow -> Set.Set Component
workflowDomain (Run r)     = nrDomain r
workflowDomain (Seq a b)   = workflowDomain a `Set.union` workflowDomain b
workflowDomain (Par a b)   = workflowDomain a `Set.union` workflowDomain b

-- | Apply one time‐step of a NumericWorkflow
applyNumericWorkflow :: NumericWorkflow -> Double -> NState -> NState
applyNumericWorkflow (Run r)   dt st = applyNumericRule r dt st
applyNumericWorkflow (Seq a b) dt st =
  let st' = applyNumericWorkflow a dt st
  in  applyNumericWorkflow b dt st'
applyNumericWorkflow (Par a b) dt st =
  let da = workflowDomain a
      db = workflowDomain b
  in  if not (Set.null (Set.intersection da db))
        then error "NumericWorkflow: parallel overlap in domain"
        else mergeNS
               (applyNumericWorkflow a dt st)
               (applyNumericWorkflow b dt st)

-- | Merge two disjoint numeric states
mergeNS :: NState -> NState -> NState
mergeNS (NS q1 p1) (NS q2 p2) = NS (mergeSL q1 q2) (mergeSL p1 p2)

mergeSL :: SLit -> SLit -> SLit
mergeSL (SL m1) (SL m2) = SL (M.union m1 m2)
