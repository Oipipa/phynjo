module Literal
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

-- | A Literal is just a set of atomic components at a given time.
newtype Literal = Literal { getLiteral :: Set Component }
  deriving (Eq, Show)

-- | The empty literal (no components present).
emptyLiteral :: Literal
emptyLiteral = Literal Set.empty

-- | Build a literal from a list (duplicates are removed).
literalFromList :: [Component] -> Literal
literalFromList = Literal . Set.fromList

-- | Ordinary union of two literals.
unionLiteral :: Literal -> Literal -> Literal
unionLiteral (Literal s1) (Literal s2) =
  Literal (Set.union s1 s2)

-- | Disjoint union: succeeds only if the two literals share no components.
disjointUnionLiteral :: Literal -> Literal -> Maybe Literal
disjointUnionLiteral (Literal s1) (Literal s2)
  | Set.null (Set.intersection s1 s2) =
      Just (Literal (Set.union s1 s2))
  | otherwise =
      Nothing

-- | Test membership of a component in a literal.
containsLiteral :: Component -> Literal -> Bool
containsLiteral c (Literal s) = Set.member c s
