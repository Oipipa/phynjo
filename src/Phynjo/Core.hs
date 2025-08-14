{-# LANGUAGE OverloadedStrings #-}
module Phynjo.Core
  ( -- Core types
    Body(..), Component(..), System(..)
  , NState(..)
  , NumericWorkflow(..)
  , SLit(..)
  , ULiteral(..), PosLit, MomLit, MassLit, ForceLit
  , EventRule(..), EventWorkflow(..), Process(..)
  , Literal
  , Transition(..), Phenomenon
  , Action(..)

    -- Constructors and helpers
  , mkBody
  , mkSystem
  , emptyNS, insertPos, insertMom, lookupPos, lookupMom, componentsNS
  , workflowDomain, applyNumericWorkflow

    -- Scalar literals
  , emptySL, singletonSL, lookupSL, insertSL, adjustSL, keysSL

    -- Unit literals and units helpers
  , getLiteral         -- from UnitLiteral
  , emptyU, singletonU, lookupU, insertU, deleteU, keysU, mapU, zipWithU
  , scaleU, mergeU, disjointMergeU
  , (*~), (/~), kilo, gram, metre, second

    -- Boolean utilities
  , ruleToAction
  , toProcess, applyEventWorkflowWorld, applyEventWorkflowPhen
  , applyProcessWorld, applyProcessPhen
  , blGetLiteral, emptyLiteral, literalFromList, unionLiteral
  , disjointUnionLiteral, containsLiteral
  , epsilon, domain, emptyPhen, singletonPhen, unionPhen

    -- Actions
  , applyActionWorld, applyActionPhen

  , printf, intercalate, forM_, (%)
  , showFFloat, hPutStrLn, stdout

  , foldM, intercalate, showFFloat
    -- Maps
  , Map, (!)
  ) where

-- Core
import Body (Body(..), mkBody)
import Components (Component(AtomicC))
import System.SystemForces (System(..), mkSystem)

-- Numeric workflow and state
import NState
  ( NState(..)
  , emptyNS, insertPos, insertMom, lookupPos, lookupMom, componentsNS
  )
import NumericWorkflow (NumericWorkflow(..), workflowDomain, applyNumericWorkflow)

-- Scalar literals
import ScalarLiteral
  ( SLit(..)
  , emptySL, singletonSL, lookupSL, insertSL, adjustSL, keysSL
  )

-- Unit literals
import UnitLiteral
  ( ULiteral(..), emptyU, singletonU, lookupU, insertU, deleteU
  , keysU, mapU, zipWithU, scaleU, mergeU, disjointMergeU
  , PosLit, MomLit, MassLit, ForceLit
  )
import qualified UnitLiteral as UL

-- Boolean utilities
import BooleanUtils.EventRule (EventRule(..), ruleToAction)
import BooleanUtils.EventWorkflow
  ( EventWorkflow(..), toProcess, applyEventWorkflowWorld, applyEventWorkflowPhen )
import BooleanUtils.Process (Process(..), applyProcessWorld, applyProcessPhen)
import BooleanUtils.Literal
  ( Literal, emptyLiteral, literalFromList
  , unionLiteral, disjointUnionLiteral, containsLiteral
  )
import qualified BooleanUtils.Literal as BL
import BooleanUtils.Transition
  ( Transition(..), Phenomenon, epsilon, domain
  , emptyPhen, singletonPhen, unionPhen
  )

-- Actions
import Action (Action(..), applyActionWorld, applyActionPhen)

-- Units and maps re-exports needed by users
import Numeric.Units.Dimensional.Prelude ((*~), (/~), kilo, gram, metre, second)

-- Other shit 
import           Data.Map.Strict (Map, (!))
import Text.Printf           ( printf )
import Data.List             (intercalate)
import           Control.Monad          (forM_)
import           Data.Ratio             ((%))
import           Numeric                (showFFloat)
import           System.IO              (hPutStrLn, stdout)
import Control.Monad (foldM) 
import Data.List (intercalate)   
import Numeric (showFFloat) 
-- Resolve getLiteral naming
getLiteral   = UL.getLiteral
blGetLiteral = BL.getLiteral
