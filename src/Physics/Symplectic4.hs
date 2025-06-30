-- src/Physics/Symplectic4.hs
{-# LANGUAGE NamedFieldPuns #-}

module Physics.Symplectic4
  ( symplectic4
  ) where

import Physics.DriftNR     (driftNR)
import Physics.ForceNR     (forceNR)
import Physics.Force       (Force)
import NRune               (NRune(..))
import NSpell              (NSpell(..))
import Components          (Component)
import qualified Data.Set        as S

-- | 4th‐order symplectic (Forest–Ruth) splitting in 1D.
--   drift(c1·dt) → kick(d1·dt) → drift(c2·dt) → kick(d2·dt)
--   → drift(c2·dt) → kick(d1·dt) → drift(c1·dt)
symplectic4
  :: Double                -- ^ full Δt
  -> [(Component,Double)]  -- ^ bodies with masses
  -> Force                 -- ^ force to apply
  -> NSpell
symplectic4 dt masses f =
  let θ  = 1 / (2 - 2 ** (1/3))
      c1 = θ / 2
      c2 = (1 - θ) / 2
      d1 = θ
      d2 = 1 - 2 * θ

      -- Domain of all bodies
      dom = S.fromList (map fst masses)

      -- Sequence of (rune‐constructor, fraction) pairs.
      -- driftNR          :: [(c,m)] -> NRune
      -- forceNR f        :: [(c,m)] -> NRune
      steps :: [ ([(Component,Double)] -> NRune, Double) ]
      steps =
        [ (driftNR,    c1)
        , (forceNR f,  d1)
        , (driftNR,    c2)
        , (forceNR f,  d2)
        , (driftNR,    c2)
        , (forceNR f,  d1)
        , (driftNR,    c1)
        ]

      -- Turn each (rfun, α) into an NRun slice
      mkSlice (rfun, α) =
        NRun NR
          { domainN = dom
          , stepN   = \_ st -> stepN (rfun masses) (α * dt) st
          }

  in foldr1 NSeq (map mkSlice steps)
