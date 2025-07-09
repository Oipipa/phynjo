{-# LANGUAGE NamedFieldPuns #-}

module SymbolicPhysics.SRune
  ( SRune(..)
  , applySRuneWorld
  ) where

import qualified Data.Set as S
import Components (Component)
import SymbolicPhysics.SState (SState)
import SymbolicPhysics.SymbolicD (Expr)

data SRune = SR
  { domainS :: S.Set Component
  , stepS   :: Expr    
            -> SState  
            -> SState  
  }

applySRuneWorld :: SRune -> Expr -> SState -> SState
applySRuneWorld SR{stepS} dt st = stepS dt st
