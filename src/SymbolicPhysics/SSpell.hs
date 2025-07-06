{-# LANGUAGE NamedFieldPuns #-}

module SymbolicPhysics.SSpell
  ( SSpell(..)
  , domainSpell
  , applySSpellWorld
  , mergeSStates
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Components              (Component)
import SymbolicPhysics.SymbolicD (Expr)
import SymbolicPhysics.SRune   (SRune(..), applySRuneWorld)
import SymbolicPhysics.SState  (SState(..))


data SSpell
  = SRun SRune
  | SSeq SSpell SSpell
  | SPar SSpell SSpell        -- parallel, must be disjoint domains

domainSpell :: SSpell -> S.Set Component
domainSpell (SRun r)     = domainS r
domainSpell (SSeq a b)   = domainSpell a `S.union` domainSpell b
domainSpell (SPar a b)   = domainSpell a `S.union` domainSpell b

applySSpellWorld :: SSpell -> Expr -> SState -> SState
applySSpellWorld (SRun r)     dt st = applySRuneWorld r dt st
applySSpellWorld (SSeq a b)   dt st =
  let st' = applySSpellWorld a dt st
  in  applySSpellWorld b dt st'
applySSpellWorld (SPar a b)   dt st =
  let da = domainSpell a
      db = domainSpell b
      stA = applySSpellWorld a dt st
      stB = applySSpellWorld b dt st
  in if not (S.null (S.intersection da db))
       then error "SSpell: parallel sub-spells overlap in domain"
       else mergeSStates da stA db stB st

-- Merge only the updated keys from each sub-spell, fill rest from original state
mergeSStates :: S.Set Component -> SState -> S.Set Component -> SState -> SState -> SState
mergeSStates dom1 (SState m1) dom2 (SState m2) (SState orig) =
  let m1' = restrict dom1 m1
      m2' = restrict dom2 m2
      merged = M.union m1' (M.union m2' orig)
  in SState merged

restrict :: Ord k => S.Set k -> M.Map k v -> M.Map k v
restrict ks m = M.filterWithKey (\k _ -> k `S.member` ks) m
