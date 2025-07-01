module Constraint
  ( Invariant
  , preservesInvariant
  ) where

import Literal            (Literal)
import EventWorkflow      (EventWorkflow, applyEventWorkflowWorld)

-- | An invariant is a predicate on system state (a Literal).
type Invariant = Literal -> Bool

-- | A workflow preserves an invariant if the invariant holds on both
--   the initial and the final flag‐set after running the workflow.
preservesInvariant
  :: EventWorkflow  -- ^ the workflow to check
  -> Int            -- ^ starting tick
  -> Literal        -- ^ starting flags
  -> Invariant      -- ^ the invariant predicate
  -> Bool
preservesInvariant wf t0 st inv =
  let (_, st') = applyEventWorkflowWorld wf t0 st
  in inv st && inv st'
