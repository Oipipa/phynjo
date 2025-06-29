-- src/System/SystemForces.hs
{-# LANGUAGE NamedFieldPuns #-}

module System.SystemForces
  ( System(..)
  , mkSystem
  ) where

import qualified Data.Map.Strict             as M
import           Numeric.Units.Dimensional.Prelude ((/~), kilo, gram)
import           UnitLiteral                  (MassLit, PosLit, MomLit, getLiteral)
import qualified System                      as Core    (System(..), bodies)
import           Body                         (Body)
import           Components                   (Component)

-- | A fast-lookup mass map (in raw kilograms).
type MassMap = M.Map Component Double

-- | Extended system carrying both the unit‐checked literals and a raw MassMap.
data System = System
  { sMass    :: MassLit   -- ^ mass in units
  , sPos     :: PosLit    -- ^ positions
  , sMom     :: MomLit    -- ^ momenta
  , sMassMap :: MassMap   -- ^ raw masses in kg
  }
  deriving (Eq, Show)

-- | Build from a non‐empty list of Bodies.
mkSystem :: [Body] -> System
mkSystem bs
  | null bs   = error "System.SystemForces.mkSystem: empty body list"
  | otherwise =
      let coreSys = Core.bodies bs         -- Core.System
          massLit = Core.sMass coreSys
          posLit  = Core.sPos  coreSys
          momLit  = Core.sMom  coreSys

          -- Map Component -> Quantity DMass Double
          rawMap  = getLiteral massLit

          -- Convert each quantity to raw Double (kg)
          massMap = M.fromList
            [ (c, q /~ (kilo gram))
            | (c, q) <- M.toList rawMap
            ]
      in System { sMass    = massLit
                , sPos     = posLit
                , sMom     = momLit
                , sMassMap = massMap
                }
