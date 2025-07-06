module Action
  ( Action(..)
  , applyActionWorld
  , applyActionPhen
  ) where

import Literal    (Literal)
import Transition (Phenomenon)
import qualified Data.Set as Set

data Action = Action
  { aWorld :: Int -> Literal -> (Int, Literal)
  , aPhen  :: Int -> Literal -> Phenomenon
  }

-- | Sequentially apply a list of actions to get the final (tick, literal).
applyActionWorld
  :: [Action]   -- ^ actions in sequence
  -> Int        -- ^ starting tick
  -> Literal    -- ^ starting literal
  -> (Int, Literal)
applyActionWorld []       t l = (t, l)
applyActionWorld (a:as)   t l =
  let (t', l') = aWorld a t l
  in applyActionWorld as t' l'

-- | Sequentially collect all phenomena from a list of actions.
--   Note: for sequential composition we simply union all transitions.
applyActionPhen
  :: [Action]   -- ^ actions in sequence
  -> Int        -- ^ starting tick
  -> Literal    -- ^ starting literal
  -> Phenomenon
applyActionPhen []       _ l = Set.empty
applyActionPhen (a:as)   t l =
  let (t', l') = aWorld a t l
      φ1        = aPhen  a t l
      φrest     = applyActionPhen as t' l'
  in Set.union φ1 φrest
