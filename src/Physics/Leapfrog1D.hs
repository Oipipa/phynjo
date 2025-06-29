-- src/Physics/Leapfrog1D.hs
{-# LANGUAGE NamedFieldPuns #-}

module Physics.Leapfrog1D
  ( leapfrog1D
  ) where

import Physics.DriftNR     (driftNR)
import Physics.ForceNR     (forceNR)
import Physics.Force       (Force)
import NRune               (NRune(..))
import NSpell              (NSpell(..))
import Components          (Component)
import qualified Data.Set     as S

-- | 1-D velocity–Verlet step:
--     drift(½·dt) → kick(dt) → drift(½·dt)
--   'masses' is list of (body, mass in kg), 'f' the Force.
leapfrog1D
  :: Double                -- ^ full Δt
  -> [(Component,Double)]  -- ^ bodies with masses
  -> Force                 -- ^ force to apply
  -> NSpell
leapfrog1D dt masses f =
  let half = dt / 2
      -- base runes
      driftRune = driftNR masses
      forceRune = forceNR f masses

      -- shared domain
      dom = S.fromList (map fst masses)

      -- override dt for half-step drift
      driftHalf = NR
        { domainN = dom
        , stepN   = \_ st -> stepN driftRune half st
        }

      -- full-step kick uses dt passed in
      kickFull  = NR
        { domainN = dom
        , stepN   = \_ st -> stepN forceRune dt st
        }
  in  NSeq (NRun driftHalf)
           (NSeq (NRun kickFull)
                 (NRun driftHalf))
