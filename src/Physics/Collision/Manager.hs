{-# LANGUAGE Safe #-}

module Physics.Collision.Manager
  ( BroadPhase(..)
  , CollisionManager
  , buildManager
  , updateManager
  , runBroadPhase
  ) where

import Components                      (Component)
import Physics.Collision.Types         (ComponentBB)
import qualified Physics.Collision.BroadPhase.SweepAndPrune as SAP
import qualified Physics.Collision.BroadPhase.UniformGrid    as UG
import Data.Set                        (Set)
import qualified Data.Set                            as S

data BroadPhase
  = BP_SAP
  | BP_Grid { gridCellSize :: Double }
  deriving (Eq, Show)

data CollisionManager
  = CM_SAP  { cmSAP  :: SAP.SAP }
  | CM_Grid { cmGrid :: UG.UniformGrid }

buildManager :: BroadPhase -> [ComponentBB] -> CollisionManager
buildManager BP_SAP     bbs = CM_SAP  (SAP.initSAP bbs)
buildManager (BP_Grid s) bbs = CM_Grid (UG.initGrid s  bbs)

updateManager :: CollisionManager -> [ComponentBB] -> CollisionManager
updateManager (CM_SAP sap) bbs = CM_SAP  (SAP.updateSAP sap bbs)
updateManager (CM_Grid ug)  bbs = CM_Grid (UG.updateGrid ug  bbs)

runBroadPhase :: CollisionManager -> Set (Component, Component)
runBroadPhase (CM_SAP sap) = SAP.potentialPairsSAP sap
runBroadPhase (CM_Grid ug) = UG.allPairsGrid ug
