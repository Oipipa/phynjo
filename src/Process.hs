module Process
  ( Process(..)
  , applyProcessWorld
  , applyProcessPhen
  ) where

import Action      (Action(..))
import Literal     (Literal, unionLiteral)
import Transition  (Phenomenon, unionPhen)
import qualified Data.Set as Set

-- | A Process is either a single Action, a sequential composition, or a parallel composition.
data Process
  = PAct Action
  | PSeq Process Process
  | PPar Process Process

-- | World‐state semantics of processes
applyProcessWorld :: Process -> Int -> Literal -> (Int, Literal)
applyProcessWorld (PAct a)     t l = aWorld a t l
applyProcessWorld (PSeq p1 p2) t l =
  let (t1, l1) = applyProcessWorld p1 t l
  in applyProcessWorld p2 t1 l1
applyProcessWorld (PPar p1 p2) t l =
  let (t1, l1) = applyProcessWorld p1 t l
      (t2, l2) = applyProcessWorld p2 t l
      t'       = max t1 t2
      l'       = unionLiteral l1 l2
  in (t', l')

-- | Phenomenon semantics of processes
applyProcessPhen :: Process -> Int -> Literal -> Phenomenon
applyProcessPhen (PAct a)     t l = aPhen a t l
applyProcessPhen (PSeq p1 p2) t l =
  let ph1      = applyProcessPhen p1 t l
      (t1, l1) = applyProcessWorld p1 t l
      ph2      = applyProcessPhen p2 t1 l1
  in Set.union ph1 ph2
applyProcessPhen (PPar p1 p2) t l =
  let ph1 = applyProcessPhen p1 t l
      ph2 = applyProcessPhen p2 t l
  in case unionPhen ph1 ph2 of
       Just ph -> ph
       Nothing -> error "Parallel processes produced overlapping phenomena"
