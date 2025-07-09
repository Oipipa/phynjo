{-# LANGUAGE RecordWildCards #-}

module Physics.Sim.Compose
  ( 
    composeRRune
  , forceRune
  ) where

import           Data.Set                 (Set)
import qualified Data.Set                 as S
import qualified Data.Map.Strict          as M

import           Components               (Component)
import           Physics.Forces.Force3D          (Force3D)
import qualified Physics.Forces.Extra    as F
import           Physics.RigidBodyUtilities.Rigid3DNR        (RRune (..), kickForce3D)

composeRRune :: [RRune] -> RRune
composeRRune [] = error "composeRRune: empty list"
composeRRune rs =
  let dom = S.unions (map domainR rs)
      step dt st = foldl (\s r -> stepR r dt s) st rs
  in RR { domainR = dom, stepR = step }


forceRune
  :: [(Component, Double)]        -- ^ mass map (kg)
  -> [(Component, InertiaTensor)] -- ^ body-space inertia tensors
  -> [Force3D]                    -- ^ one or more fields
  -> RRune
forceRune masses inertias [] =
  error "forceRune: need at least one Force3D"
forceRune masses inertias fs =
  kickForce3D masses inertias (F.sumForces3D fs)

-- Local copy (type only) to avoid depending on internal modules.
type InertiaTensor = ( (Double,Double,Double)
                     , (Double,Double,Double)
                     , (Double,Double,Double) )
