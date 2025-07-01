{-# LANGUAGE NamedFieldPuns #-}

module SymbolicPhysics.SRune
  ( SRune(..)
  , applySRuneWorld
  ) where

import qualified Data.Set as S
import Components (Component)
import SymbolicPhysics.SState (SState)
import SymbolicPhysics.SymbolicD (Expr)

----------------------------------------------------------------------
-- Data type
----------------------------------------------------------------------

data SRune = SR
  { domainS :: S.Set Component
  , stepS   :: Expr    -- ^ Δt (can be a variable, constant, etc)
            -> SState  -- ^ current state
            -> SState  -- ^ next state
  }

----------------------------------------------------------------------
-- Helper to run a rune once
----------------------------------------------------------------------

applySRuneWorld :: SRune -> Expr -> SState -> SState
applySRuneWorld SR{stepS} dt st = stepS dt st
