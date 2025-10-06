{-# LANGUAGE NamedFieldPuns #-}

module Base.NumericWorkflow
  ( NumericWorkflow(..)
  , workflowDomain
  , applyNumericWorkflow
  ) where

import           Base.Components         (Component)
import qualified Data.Set           as Set
import qualified Data.Map.Strict    as M

import           Base.NumericRule        (NumericRule(..), applyNumericRule)
import           Base.NState             (NState(..))
import           Base.ScalarLiteral      (SLit(..)) 

data NumericWorkflow
  = Run NumericRule
  | Seq NumericWorkflow NumericWorkflow
  | Par NumericWorkflow NumericWorkflow

workflowDomain :: NumericWorkflow -> Set.Set Component
workflowDomain (Run r)     = nrDomain r
workflowDomain (Seq a b)   = workflowDomain a `Set.union` workflowDomain b
workflowDomain (Par a b)   = workflowDomain a `Set.union` workflowDomain b

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

mergeNS :: NState -> NState -> NState
mergeNS (NS q1 p1) (NS q2 p2) = NS (mergeSL q1 q2) (mergeSL p1 p2)

mergeSL :: SLit -> SLit -> SLit
mergeSL (SL m1) (SL m2) = SL (M.union m1 m2)
