{-# LANGUAGE NamedFieldPuns #-}

-- A COMPILING placeholder for the drift rune.
-- ---------------------------------------------------------------
-- We haven’t yet refactored `Rune` to accept numerical literals,
-- so this module just provides a no-op rune that type-checks
-- against the current `Literal`-based signature.  
module Physics.Drift
  ( driftRune        -- :: Double -> [(Component,Double)] -> Rune
  ) where

import Components   (Component)
import Literal      (Literal)       -- the set-based literal for now
import Rune         (Rune (..))
import qualified Data.Set as S

-- | Stub drift rune: leaves the world state unchanged.
driftRune
  :: Double                 -- ^ Δt  (ignored for now)
  -> [(Component,Double)]   -- ^ (component, mass) list (ignored)
  -> Rune
driftRune _ _ =
  Rune
    { domainRune = S.empty   -- affects no components yet
    , fRune      = id        -- state unchanged
    , gRune      = \_ _ -> mempty
    }
