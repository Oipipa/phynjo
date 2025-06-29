{-# LANGUAGE NamedFieldPuns #-}

-- | Numeric Spell: composition of `NRune`s over the numeric state (`NState`).
--   Supports sequential and parallel composition; parallel requires that the
--   two sub-spells act on disjoint component domains.
module NSpell
  ( NSpell(..)
  , domainS
  , applyNSpellWorld
  ) where

import Components        (Component)
import qualified Data.Set        as S
import qualified Data.Map.Strict as M

import NRune              (NRune(..), applyNRuneWorld)
import NState             (NState(..))
import ScalarLiteral      (SLit(..))   -- exposes the constructor ‘SL’

----------------------------------------------------------------------
-- Data type
----------------------------------------------------------------------

data NSpell
  = NRun NRune
  | NSeq NSpell NSpell
  | NPar NSpell NSpell        -- parallel, must be disjoint domains

----------------------------------------------------------------------
-- Domain of a spell (set of components it may touch)
----------------------------------------------------------------------

domainS :: NSpell -> S.Set Component
domainS (NRun r)     = domainN r
domainS (NSeq a b)   = domainS a `S.union` domainS b
domainS (NPar a b)   = domainS a `S.union` domainS b

----------------------------------------------------------------------
-- Interpreter
----------------------------------------------------------------------

applyNSpellWorld :: NSpell -> Double -> NState -> NState
applyNSpellWorld (NRun r)     dt st = applyNRuneWorld r dt st
applyNSpellWorld (NSeq a b)   dt st =
  let st' = applyNSpellWorld a dt st
  in  applyNSpellWorld b dt st'
applyNSpellWorld (NPar a b)   dt st =
  let da = domainS a
      db = domainS b
  in if not (S.null (S.intersection da db))
       then error "NSpell: parallel sub-spells overlap in domain"
       else mergeStates (applyNSpellWorld a dt st)
                        (applyNSpellWorld b dt st)

----------------------------------------------------------------------
-- Merge two disjoint numeric states
----------------------------------------------------------------------

mergeStates :: NState -> NState -> NState
mergeStates (NS q1 p1) (NS q2 p2) = NS (mergeSL q1 q2) (mergeSL p1 p2)

-- ScalarLiteral union (domains disjoint ⇒ maps have distinct keys)
mergeSL :: SLit -> SLit -> SLit
mergeSL (SL m1) (SL m2) = SL (M.union m1 m2)
