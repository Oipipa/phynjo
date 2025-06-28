module Constraint
  ( Invariant
  , preservesInvariant
  ) where

import Literal (Literal)
import Spell   (Spell, applySpellWorld)

-- | An invariant is a predicate on system state (a Literal).
type Invariant = Literal -> Bool

-- | A spell preserves an invariant if the invariant holds on both
--   the initial and the final state after running the spell.
preservesInvariant
  :: Spell       -- ^ the spell to check
  -> Int         -- ^ starting tick (ignored by most invariants)
  -> Literal     -- ^ starting state
  -> Invariant   -- ^ the invariant predicate
  -> Bool
preservesInvariant spell t0 st inv =
  let (_, st') = applySpellWorld spell t0 st
  in inv st && inv st'
