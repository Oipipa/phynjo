{-# LANGUAGE NamedFieldPuns #-}

module Base.NumericRule
  ( NumericRule(..)
  , applyNumericRule
  ) where

import qualified Data.Set   as Set
import           Base.Components  (Component)
import           Base.NState      (NState)

data NumericRule = NumericRule
  { nrDomain :: Set.Set Component
  , nrStep   :: Double -> NState -> NState
  }

applyNumericRule :: NumericRule -> Double -> NState -> NState
applyNumericRule NumericRule{nrStep} dt st = nrStep dt st
