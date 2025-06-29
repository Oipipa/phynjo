{-# LANGUAGE NamedFieldPuns #-}

-- | Numeric rune: a single-tick transformer on an ‘NState’.
--   The step receives a time step Δt (Double, seconds for now)
--   and produces the next state.  This is independent of the
--   old Boolean-literal ‘Rune’.

module NRune
  ( NRune(..)
  , applyNRuneWorld
  ) where

import qualified Data.Set as S
import Components (Component)
import NState     (NState)

----------------------------------------------------------------------
-- Data type
----------------------------------------------------------------------

data NRune = NR
  { domainN :: S.Set Component           -- ^ components this rune may touch
  , stepN   :: Double                    -- ^ Δt
             -> NState                   -- ^ current state
             -> NState                   -- ^ next state
  }

----------------------------------------------------------------------
-- Helper to run a rune once
----------------------------------------------------------------------

applyNRuneWorld :: NRune -> Double -> NState -> NState
applyNRuneWorld NR{stepN} dt st = stepN dt st
