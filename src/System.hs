{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedFieldPuns   #-}

module System
  ( System(..)
  , bodies          -- ^ build a system from a list of bodies
  , (<+>)           -- ^ safe union of two disjoint subsystems
  ) where

import Body
import UnitLiteral
import qualified Data.Maybe as Mb

----------------------------------------------------------------------
-- Aggregate record
----------------------------------------------------------------------

data System = Sys
  { sMass :: MassLit    -- ^ mass of every component
  , sPos  :: PosLit     -- ^ positions
  , sMom  :: MomLit     -- ^ momenta
  }
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- convert a single Body to a singleton System
bodyToSys :: Body -> System
bodyToSys Body{mass, pos0, mom0} = Sys mass pos0 mom0

-- internal unsafe merge (assumes keys disjoint)
unsafeMerge :: System -> System -> System
unsafeMerge (Sys m1 q1 p1) (Sys m2 q2 p2) =
  Sys (mergeU m1 m2) (mergeU q1 q2) (mergeU p1 p2)

----------------------------------------------------------------------
-- API
----------------------------------------------------------------------

-- | Build a system from a non-empty list of bodies (assumes unique ids).
bodies :: [Body] -> System
bodies []     = error "System.bodies: empty list"
bodies (b:bs) = foldl unsafeMerge (bodyToSys b) (map bodyToSys bs)

-- | Disjoint union; returns Nothing on duplicate component keys.
(<+>) :: System -> System -> Maybe System
Sys m1 q1 p1 <+> Sys m2 q2 p2 = do
  m <- disjointMergeU m1 m2
  q <- disjointMergeU q1 q2
  p <- disjointMergeU p1 p2
  pure (Sys m q p)
