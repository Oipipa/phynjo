module Spell
  ( Spell(..)
  , toProcess
  , applySpellWorld
  , applySpellPhen
  ) where

import Rune    (Rune, toAction)
import Process (Process(..), applyProcessWorld, applyProcessPhen)
import Literal (Literal)
import Transition (Phenomenon)

-- | Spells are like Processes but built over Runes.
data Spell
  = SRun Rune
  | SSeq Spell Spell
  | SPar Spell Spell

-- | Convert a Spell into a Process of Actions.
toProcess :: Spell -> Process
toProcess (SRun r)       = PAct (toAction r)
toProcess (SSeq s1 s2)   = PSeq (toProcess s1) (toProcess s2)
toProcess (SPar s1 s2)   = PPar (toProcess s1) (toProcess s2)

-- | World‐state semantics of a Spell, via its Process.
applySpellWorld
  :: Spell     -- ^ the spell
  -> Int       -- ^ starting tick
  -> Literal   -- ^ starting literal
  -> (Int, Literal)
applySpellWorld spell = applyProcessWorld (toProcess spell)

-- | Phenomenon semantics of a Spell, via its Process.
applySpellPhen
  :: Spell     -- ^ the spell
  -> Int       -- ^ starting tick
  -> Literal   -- ^ starting literal
  -> Phenomenon
applySpellPhen spell = applyProcessPhen (toProcess spell)
