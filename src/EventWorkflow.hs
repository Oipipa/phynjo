{-# LANGUAGE NamedFieldPuns #-}

-- | Event‐driven workflow: composition of atomic EventRules into
--   sequential or parallel behaviors.
module EventWorkflow
  ( EventWorkflow(..)
  , toProcess
  , applyEventWorkflowWorld
  , applyEventWorkflowPhen
  ) where

import           EventRule       (EventRule, ruleToAction)
import           Process         (Process(..), applyProcessWorld, applyProcessPhen)
import           Literal         (Literal)
import           Transition      (Phenomenon)

-- | A little workflow DSL built on top of EventRule→Action→Process.
data EventWorkflow
  = ERun EventRule
  | ESeq EventWorkflow EventWorkflow
  | EPar EventWorkflow EventWorkflow

-- | Lower an EventWorkflow into a Process of Actions.
toProcess :: EventWorkflow -> Process
toProcess (ERun r)     = PAct (ruleToAction r)
toProcess (ESeq w1 w2) = PSeq (toProcess w1) (toProcess w2)
toProcess (EPar w1 w2) = PPar (toProcess w1) (toProcess w2)

-- | Advance the boolean world‐state by one “tick” through the workflow.
applyEventWorkflowWorld
  :: EventWorkflow
  -> Int        -- ^ starting tick
  -> Literal    -- ^ starting flag‐set
  -> (Int, Literal)
applyEventWorkflowWorld wf = applyProcessWorld (toProcess wf)

-- | Collect all events emitted during one run of the workflow.
applyEventWorkflowPhen
  :: EventWorkflow
  -> Int
  -> Literal
  -> Phenomenon
applyEventWorkflowPhen wf = applyProcessPhen (toProcess wf)
