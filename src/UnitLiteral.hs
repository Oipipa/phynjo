{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Unit-checked literals: each component is annotated with a
--   ‘dimensional’ quantity of *one* fixed dimension @u@.
--   The module is independent of the old ‘Literal’ API; adopt it gradually.
module UnitLiteral
  ( ULiteral(..)
  , getLiteral
  , emptyU
  , singletonU
  , lookupU
  , insertU
  , deleteU
  , keysU
  , mapU
  , zipWithU
  , scaleU
  , mergeU
  , disjointMergeU
  -- handy physics aliases
  , PosLit , MomLit , MassLit , ForceLit
  ) where

import Components                      (Component)
import Data.Map.Strict                 (Map)
import qualified Data.Map.Strict       as M
import Numeric.Units.Dimensional.Prelude
         ( Quantity, (+), (*), DOne
         , DLength, DMass, DTime, DForce
         , KnownDimension )
import qualified Numeric.Units.Dimensional.Prelude as D

----------------------------------------------------------------------
-- Core newtype
----------------------------------------------------------------------

newtype ULiteral u = ULit { unU :: Map Component (Quantity u Double) }

deriving instance Eq (ULiteral u)

instance KnownDimension u => Show (ULiteral u) where
  show (ULit m) = "ULit " ++ show m

----------------------------------------------------------------------
-- Constructors / queries
----------------------------------------------------------------------

emptyU :: ULiteral u
emptyU = ULit M.empty

singletonU :: Component -> Quantity u Double -> ULiteral u
singletonU c q = ULit (M.singleton c q)

lookupU :: Component -> ULiteral u -> Maybe (Quantity u Double)
lookupU c (ULit m) = M.lookup c m

insertU :: Component -> Quantity u Double -> ULiteral u -> ULiteral u
insertU c q (ULit m) = ULit (M.insert c q m)

deleteU :: Component -> ULiteral u -> ULiteral u
deleteU c (ULit m) = ULit (M.delete c m)

keysU :: ULiteral u -> [Component]
keysU (ULit m) = M.keys m

getLiteral :: ULiteral u -> Map Component (Quantity u Double)
getLiteral = unU

----------------------------------------------------------------------
-- Functor-like utilities
----------------------------------------------------------------------

mapU :: (Quantity u Double -> Quantity u Double) -> ULiteral u -> ULiteral u
mapU f (ULit m) = ULit (M.map f m)

zipWithU
  :: (Quantity u Double -> Quantity u Double -> Quantity u Double)
  -> ULiteral u -> ULiteral u -> ULiteral u
zipWithU f (ULit a) (ULit b) = ULit (M.unionWith f a b)

-- | Multiply every quantity by a dimension-less scalar
scaleU :: Quantity DOne Double -> ULiteral u -> ULiteral u
scaleU k = mapU (k D.*)

----------------------------------------------------------------------
-- Union helpers
----------------------------------------------------------------------

mergeU :: ULiteral u -> ULiteral u -> ULiteral u
mergeU = zipWithU (D.+)

disjointMergeU :: ULiteral u -> ULiteral u -> Maybe (ULiteral u)
disjointMergeU (ULit a) (ULit b)
  | M.null (M.intersection a b) = Just (ULit (M.union a b))
  | otherwise                   = Nothing

----------------------------------------------------------------------
-- Handy physics aliases
----------------------------------------------------------------------

type PosLit   = ULiteral DLength
type MomLit   = ULiteral (DMass D.* DLength D./ DTime)
type MassLit  = ULiteral DMass
type ForceLit = ULiteral DForce
