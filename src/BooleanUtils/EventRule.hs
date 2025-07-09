module BooleanUtils.EventRule
  ( EventRule(..)
  , ruleToAction
  ) where

import           Components   (Component)
import           BooleanUtils.Literal      (Literal, getLiteral, literalFromList)
import qualified BooleanUtils.Literal      as L
import           BooleanUtils.Transition   (Phenomenon)
import           Action       (Action(..))
import qualified Data.Set     as Set

data EventRule = EventRule
  { erDomain :: Set.Set Component
  , erStep   :: Literal -> Literal
  , erEvents :: Int -> Literal -> Phenomenon
  }

ruleToAction :: EventRule -> Action
ruleToAction r = Action
  { aWorld = \t worldLit ->
      let fullSet    = getLiteral worldLit
          dom        = erDomain r
          domLit     = literalFromList
                         (Set.toList (Set.intersection fullSet dom))
          outsideLit = literalFromList
                         (Set.toList (Set.difference fullSet dom))
          updatedDom = erStep r domLit
          newLit     = L.unionLiteral updatedDom outsideLit
      in (t + 1, newLit)

  , aPhen  = \t worldLit ->
      let fullSet = getLiteral worldLit
          dom     = erDomain r
          domLit  = literalFromList
                      (Set.toList (Set.intersection fullSet dom))
      in erEvents r t domLit
  }
