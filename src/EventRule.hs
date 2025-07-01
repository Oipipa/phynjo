module EventRule
  ( EventRule(..)
  , ruleToAction
  ) where

import           Components   (Component)
import           Literal      (Literal, getLiteral, literalFromList)
import qualified Literal      as L
import           Transition   (Phenomenon)
import           Action       (Action(..))
import qualified Data.Set     as Set

-- | An atomic event‐rule: a scope of flags it may read/write,
--   a pure step function on that scope, and an event generator.
data EventRule = EventRule
  { erDomain :: Set.Set Component
    -- ^ Which flags this rule may affect
  , erStep   :: Literal -> Literal
    -- ^ How the flags in its domain are updated
  , erEvents :: Int -> Literal -> Phenomenon
    -- ^ What events it emits at a given tick and input flags
  }

-- | Embed an 'EventRule' into the full‐world 'Action', so that
--   only its 'erDomain' is rewritten (others pass through).
ruleToAction :: EventRule -> Action
ruleToAction r = Action
  { aWorld = \t worldLit ->
      let fullSet    = getLiteral worldLit
          dom        = erDomain r
          -- extract domain and outside
          domLit     = literalFromList
                         (Set.toList (Set.intersection fullSet dom))
          outsideLit = literalFromList
                         (Set.toList (Set.difference fullSet dom))
          -- apply the local step…
          updatedDom = erStep r domLit
          -- …and rebuild the full literal
          newLit     = L.unionLiteral updatedDom outsideLit
      in (t + 1, newLit)

  , aPhen  = \t worldLit ->
      let fullSet = getLiteral worldLit
          dom     = erDomain r
          domLit  = literalFromList
                      (Set.toList (Set.intersection fullSet dom))
      in erEvents r t domLit
  }
