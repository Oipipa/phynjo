module Constraint
  ( Invariant
  , preservesInvariant
  ) where

import BooleanUtils.Literal            (Literal)
import BooleanUtils.EventWorkflow      (EventWorkflow, applyEventWorkflowWorld)

type Invariant = Literal -> Bool

preservesInvariant
  :: EventWorkflow 
  -> Int 
  -> Literal 
  -> Invariant 
  -> Bool
preservesInvariant wf t0 st inv =
  let (_, st') = applyEventWorkflowWorld wf t0 st
  in inv st && inv st'
