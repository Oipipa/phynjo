module Transition
  ( Transition(..)
  , Phenomenon
  , epsilon
  , domain
  , emptyPhen
  , singletonPhen
  , unionPhen
  ) where

import Components (Component)
import Data.Set    (Set)
import qualified Data.Set as Set

-- | A single‐tick change of one component from t → t+1.
data Transition = Transition
  { src   :: Component  -- ^ source component at time t
  , tgt   :: Component  -- ^ target component at time t+1
  , tfrom :: Int        -- ^ tick t
  , tto   :: Int        -- ^ tick t+1
  } deriving (Eq, Ord, Show)

-- | A phenomenon is a set of independent transitions in one tick‐step.
type Phenomenon = Set Transition

-- | The “no‐change” transition for component c at time t.
epsilon :: Component -> Int -> Transition
epsilon c t = Transition c c t (t + 1)

domain :: Transition -> Set Component
domain (Transition s t _ _)
  | s == t    = Set.empty
  | otherwise = Set.fromList [s, t]

-- | The empty phenomenon (no transitions).
emptyPhen :: Phenomenon
emptyPhen = Set.empty

-- | A singleton phenomenon from one transition.
singletonPhen :: Transition -> Phenomenon
singletonPhen = Set.singleton

-- | Disjoint‐union of two phenomena: succeeds iff their domains don't overlap.
unionPhen :: Phenomenon -> Phenomenon -> Maybe Phenomenon
unionPhen p1 p2
  | Set.null (Set.intersection dom1 dom2) = Just (Set.union p1 p2)
  | otherwise                              = Nothing
 where
  dom1 = Set.unions (map domain (Set.toList p1))
  dom2 = Set.unions (map domain (Set.toList p2))
