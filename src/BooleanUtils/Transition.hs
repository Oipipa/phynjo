module BooleanUtils.Transition
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

data Transition = Transition
  { src   :: Component 
  , tgt   :: Component 
  , tfrom :: Int 
  , tto   :: Int 
  } deriving (Eq, Ord, Show)

type Phenomenon = Set Transition

epsilon :: Component -> Int -> Transition
epsilon c t = Transition c c t (t + 1)

domain :: Transition -> Set Component
domain (Transition s t _ _)
  | s == t    = Set.empty
  | otherwise = Set.fromList [s, t]

emptyPhen :: Phenomenon
emptyPhen = Set.empty

singletonPhen :: Transition -> Phenomenon
singletonPhen = Set.singleton

unionPhen :: Phenomenon -> Phenomenon -> Maybe Phenomenon
unionPhen p1 p2
  | Set.null (Set.intersection dom1 dom2) = Just (Set.union p1 p2)
  | otherwise                              = Nothing
 where
  dom1 = Set.unions (map domain (Set.toList p1))
  dom2 = Set.unions (map domain (Set.toList p2))
