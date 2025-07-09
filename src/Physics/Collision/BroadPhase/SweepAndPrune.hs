{-# LANGUAGE Safe #-}

module Physics.Collision.BroadPhase.SweepAndPrune
  ( SAP
  , initSAP
  , updateSAP
  , potentialPairsSAP
  ) where

import Physics.Collision.Types (ComponentBB, BoundingVolume(..), AABB(..))
import Physics.Math.Vec3Util   (Vec3)
import Components               (Component)
import Data.List                (sortOn)
import qualified Data.Set as S

data SAP = SAP { entries :: [ComponentBB] }

initSAP :: [ComponentBB] -> SAP
initSAP bbs = SAP bbs

updateSAP :: SAP -> [ComponentBB] -> SAP
updateSAP _ _bbs = SAP _bbs

potentialPairsSAP :: SAP -> S.Set (Component,Component)
potentialPairsSAP (SAP bbs) =
  let axisPairs f = sweep1D [ (c, f mn, f mx) | (c, BB_AABB (AABB mn mx)) <- bbs ]
      pX = axisPairs (\(x,_,_) -> x)
      pY = axisPairs (\(_,y,_) -> y)
      pZ = axisPairs (\(_,_,z) -> z)
  in pX `S.intersection` pY `S.intersection` pZ

type Interval1D = (Component, Double, Double)

data EndPoint = EP Component Double Bool
  -- Bool = True for low, False for high

sweep1D :: [Interval1D] -> S.Set (Component,Component)
sweep1D intervals =
  let endpoints = sortOn (\(EP _ pos _) -> pos) $
                   concat [ [EP c lo True, EP c hi False] | (c,lo,hi) <- intervals ]
      go _ [] acc = acc
      go active (EP c _ isStart : es) acc
        | isStart  =
            let newPairs = [ orderPair c d | d <- S.toList active ]
            in go (S.insert c active) es (acc `S.union` S.fromList newPairs)
        | otherwise = go (S.delete c active) es acc
  in go S.empty endpoints S.empty

orderPair :: Component -> Component -> (Component,Component)
orderPair a b
  | a <= b    = (a,b)
  | otherwise = (b,a)
