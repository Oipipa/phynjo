{-# LANGUAGE NamedFieldPuns #-}

module BooleanUtils.EventWorkflow
  ( EventWorkflow(..)
  , toProcess
  , applyEventWorkflowWorld
  , applyEventWorkflowPhen
  ) where

import           BooleanUtils.EventRule       (EventRule, ruleToAction)
import           BooleanUtils.Process         (Process(..), applyProcessWorld, applyProcessPhen)
import           BooleanUtils.Literal         (Literal)
import           BooleanUtils.Transition      (Phenomenon)

data EventWorkflow
  = ERun EventRule
  | ESeq EventWorkflow EventWorkflow
  | EPar EventWorkflow EventWorkflow

toProcess :: EventWorkflow -> Process
toProcess (ERun r)     = PAct (ruleToAction r)
toProcess (ESeq w1 w2) = PSeq (toProcess w1) (toProcess w2)
toProcess (EPar w1 w2) = PPar (toProcess w1) (toProcess w2)

applyEventWorkflowWorld
  :: EventWorkflow
  -> Int
  -> Literal 
  -> (Int, Literal)
applyEventWorkflowWorld wf = applyProcessWorld (toProcess wf)

applyEventWorkflowPhen
  :: EventWorkflow
  -> Int
  -> Literal
  -> Phenomenon
applyEventWorkflowPhen wf = applyProcessPhen (toProcess wf)
