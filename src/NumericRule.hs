{-# LANGUAGE NamedFieldPuns #-}

module NumericRule
  ( NumericRule(..)
  , applyNumericRule
  ) where

import qualified Data.Set   as Set
import           Components  (Component)
import           NState      (NState)

-- | A NumericRule carries:
--
--   * a domain of components it may touch
--   * a pure step function dt → oldState → newState
data NumericRule = NumericRule
  { nrDomain :: Set.Set Component
  , nrStep   :: Double -> NState -> NState
  }

-- | Apply one time-step of a NumericRule.
applyNumericRule :: NumericRule -> Double -> NState -> NState
applyNumericRule NumericRule{nrStep} dt st = nrStep dt st
