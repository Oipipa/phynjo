module Action
  ( Action(..)
  , applyActionWorld
  , applyActionPhen
  ) where

import BooleanUtils.Literal    (Literal)
import BooleanUtils.Transition (Phenomenon)
import qualified Data.Set as Set

data Action = Action
  { aWorld :: Int -> Literal -> (Int, Literal)
  , aPhen  :: Int -> Literal -> Phenomenon
  }

applyActionWorld
  :: [Action] 
  -> Int 
  -> Literal 
  -> (Int, Literal)
applyActionWorld []       t l = (t, l)
applyActionWorld (a:as)   t l =
  let (t', l') = aWorld a t l
  in applyActionWorld as t' l'

applyActionPhen
  :: [Action] 
  -> Int 
  -> Literal 
  -> Phenomenon
applyActionPhen []       _ l = Set.empty
applyActionPhen (a:as)   t l =
  let (t', l') = aWorld a t l
      phi1        = aPhen  a t l
      phirest     = applyActionPhen as t' l'
  in Set.union phi1 phirest
