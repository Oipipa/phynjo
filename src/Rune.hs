module Rune
  ( Rune(..)
  , toAction
  ) where

import Components (Component)
import Literal    (Literal, getLiteral, literalFromList)
import qualified Literal as L
import Transition (Phenomenon)
import Action     (Action(..))
import qualified Data.Set as Set

-- | An atomic rune: a domain of components, a state‐update on that domain,
--   and a phenomenon‐generation function on that domain.
data Rune = Rune
  { domainRune :: Set.Set Component
    -- ^ The static scope: which components this rune may affect.
  , fRune      :: Literal -> Literal
    -- ^ Next‐state function on the domain.
  , gRune      :: Int -> Literal -> Phenomenon
    -- ^ Phenomenon‐generation for that tick and literal.
  }

-- | Convert a Rune into an Action by embedding its local update into the
--   full-world literal: update only the domain, leave other components unchanged.
toAction :: Rune -> Action
toAction r = Action
  { aWorld = \t lit ->
      let
        -- split the literal into domain part and outside part
        fullSet      = getLiteral lit
        domSet       = domainRune r
        domLiteral   = literalFromList (Set.toList (Set.intersection fullSet domSet))
        outsideSet   = Set.difference fullSet domSet
        outsideLit   = literalFromList (Set.toList outsideSet)

        -- apply the rune's state‐update to its domain
        updatedDom   = fRune r domLiteral

        -- rebuild the full literal
        newLiteral   = L.unionLiteral updatedDom outsideLit
      in
        (t + 1, newLiteral)

  , aPhen  = \t lit ->
      -- phenomenon is generated only from the domain part
      let
        fullSet    = getLiteral lit
        domSet     = domainRune r
        domLiteral = literalFromList (Set.toList (Set.intersection fullSet domSet))
      in
        gRune r t domLiteral
  }
