{-# LANGUAGE RecordWildCards #-}

module Physics.RigidBodyUtilities.RigidState
  ( RigidState(..)
  , emptyRigid
  , insertRigid
  , lookupPosR
  , lookupOriR
  , lookupVelR
  , lookupAngVelR
  ) where

import Components            (Component)
import Physics.Math.LinearAlgebra    (Vec3)
import Physics.RigidBodyUtilities.RigidBody     (Quaternion)
import Data.Map.Strict       (Map)
import qualified Data.Map.Strict as M

data RigidState = RigidState
  { rsPos    :: Map Component Vec3
  , rsOri    :: Map Component Quaternion
  , rsVel    :: Map Component Vec3
  , rsAngVel :: Map Component Vec3
  } deriving (Eq, Show)

emptyRigid :: RigidState
emptyRigid = RigidState M.empty M.empty M.empty M.empty

insertRigid
  :: Component
  -> Vec3 
  -> Quaternion 
  -> Vec3 
  -> Vec3 
  -> RigidState
  -> RigidState
insertRigid c pos ori vel ang s@RigidState{..} =
  s { rsPos    = M.insert c pos    rsPos
    , rsOri    = M.insert c ori    rsOri
    , rsVel    = M.insert c vel    rsVel
    , rsAngVel = M.insert c ang    rsAngVel
    }

-- | Lookup functions return a default if missing.
lookupPosR    :: Component -> RigidState -> Vec3
lookupPosR    c RigidState{..} = M.findWithDefault (0,0,0)          c rsPos

lookupOriR    :: Component -> RigidState -> Quaternion
lookupOriR    c RigidState{..} = M.findWithDefault (1,0,0,0)        c rsOri

lookupVelR    :: Component -> RigidState -> Vec3
lookupVelR    c RigidState{..} = M.findWithDefault (0,0,0)          c rsVel

lookupAngVelR :: Component -> RigidState -> Vec3
lookupAngVelR c RigidState{..} = M.findWithDefault (0,0,0)          c rsAngVel
