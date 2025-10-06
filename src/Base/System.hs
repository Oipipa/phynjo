{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Base.System
  ( System(..)
  , bodies  
  , (<+>) 
  ) where

import Base.Body
import Base.UnitLiteral
import qualified Data.Maybe as Mb

data System = Sys
  { sMass :: MassLit    -- ^ mass of every component
  , sPos  :: PosLit     -- ^ positions
  , sMom  :: MomLit     -- ^ momenta
  }
  deriving (Eq, Show)

bodyToSys :: Body -> System
bodyToSys Body{mass, pos0, mom0} = Sys mass pos0 mom0

unsafeMerge :: System -> System -> System
unsafeMerge (Sys m1 q1 p1) (Sys m2 q2 p2) =
  Sys (mergeU m1 m2) (mergeU q1 q2) (mergeU p1 p2)

bodies :: [Body] -> System
bodies []     = error "System.bodies: empty list"
bodies (b:bs) = foldl unsafeMerge (bodyToSys b) (map bodyToSys bs)

(<+>) :: System -> System -> Maybe System
Sys m1 q1 p1 <+> Sys m2 q2 p2 = do
  m <- disjointMergeU m1 m2
  q <- disjointMergeU q1 q2
  p <- disjointMergeU p1 p2
  pure (Sys m q p)
