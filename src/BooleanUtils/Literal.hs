module BooleanUtils.Literal
  ( Literal
  , getLiteral
  , emptyLiteral
  , literalFromList
  , unionLiteral
  , disjointUnionLiteral
  , containsLiteral
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Components (Component)

newtype Literal = Literal { getLiteral :: Set Component }
  deriving (Eq, Show)

emptyLiteral :: Literal
emptyLiteral = Literal Set.empty

literalFromList :: [Component] -> Literal
literalFromList = Literal . Set.fromList

unionLiteral :: Literal -> Literal -> Literal
unionLiteral (Literal s1) (Literal s2) =
  Literal (Set.union s1 s2)

disjointUnionLiteral :: Literal -> Literal -> Maybe Literal
disjointUnionLiteral (Literal s1) (Literal s2)
  | Set.null (Set.intersection s1 s2) =
      Just (Literal (Set.union s1 s2))
  | otherwise =
      Nothing

containsLiteral :: Component -> Literal -> Bool
containsLiteral c (Literal s) = Set.member c s
