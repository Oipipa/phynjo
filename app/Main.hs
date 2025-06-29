{-# LANGUAGE OverloadedStrings #-}
module Main where

import Body                          (Body, mkBody)
import Components                    (Component(AtomicC))
import System.SystemForces           (mkSystem, System(..))
import UnitLiteral                   (getLiteral)
import NSpell                        (NSpell, applyNSpellWorld)
import NState                        ( NState
                                     , emptyNS
                                     , insertPos
                                     , insertMom
                                     , lookupPos
                                     , lookupMom
                                     )
import Physics.Force                 (Force(..))
import Physics.ForceDSL              (addForces)
import Numeric.Units.Dimensional.Prelude
  ( (*~)
  , (/~)
  , kilo
  , gram
  , metre
  , second
  )
import qualified Numeric.Units.Dimensional.Prelude as D
import qualified Data.Map.Strict       as M
import           Data.Map.Strict         (Map, (!))

-- Simulation parameters
dt      :: Double
dt       = 0.01

nSteps  :: Int
nSteps   = 2000

-- Spring constant & rest length
kSpring :: Double
kSpring  = 10.0

restLen :: Double
restLen   = 1.0

-- Two 1 kg bodies, initially at x=0 and x=restLen+0.1
body1, body2 :: Body
body1 = mkBody "m1" (1 *~ kilo gram) 0.0
body2 = mkBody "m2" (1 *~ kilo gram) (restLen + 0.1)

-- Build the extended system
sys :: System
sys = mkSystem [body1, body2]

-- Convert unit‐checked maps into raw Double maps for NState
initState :: NState
initState =
  let posUnitMap = getLiteral (sPos sys)   -- Map Component (Quantity DLength Double)
      momUnitMap = getLiteral (sMom sys)   -- Map Component (Quantity (DMass*DLength/DTime) Double)

      -- Define pure Units (not Quantities):
      posU = metre                                -- Unit DLength
      momU = (kilo gram) D.* (metre D./ second)  -- Unit (DMass · DLength / DTime)

      -- Strip units:
      posMap :: Map Component Double
      posMap = M.map (\q -> q /~ posU) posUnitMap

      momMap :: Map Component Double
      momMap = M.map (\q -> q /~ momU) momUnitMap

      -- Build NState
      st0 = emptyNS
      st1 = M.foldrWithKey insertPos st0 posMap
  in    M.foldrWithKey insertMom st1 momMap

-- Only a Hooke spring (no drag) so P and E are conserved
forces :: [Force]
forces = [ Spring (AtomicC "m1") (AtomicC "m2") kSpring restLen ]

-- One step: kick → drift
spell :: NSpell
spell = addForces dt forces sys

-- Full trajectory
trajectory :: [NState]
trajectory = iterate (applyNSpellWorld spell dt) initState

-- Compute total momentum p1 + p2
totalMomentum :: NState -> Double
totalMomentum st =
  lookupMom (AtomicC "m1") st
  + lookupMom (AtomicC "m2") st

-- Compute total energy (kinetic + spring potential)
totalEnergy :: NState -> Double
totalEnergy st =
  let m1 = sMassMap sys ! AtomicC "m1"
      m2 = sMassMap sys ! AtomicC "m2"
      x1 = lookupPos  (AtomicC "m1") st
      x2 = lookupPos  (AtomicC "m2") st
      p1 = lookupMom  (AtomicC "m1") st
      p2 = lookupMom  (AtomicC "m2") st
      v1 = p1 / m1
      v2 = p2 / m2
      ke = 0.5 * m1 * v1 * v1
         + 0.5 * m2 * v2 * v2
      dx = x2 - x1
      pe = 0.5 * kSpring * (dx - restLen) * (dx - restLen)
  in ke + pe

-- Print one CSV row
printRow :: Int -> NState -> IO ()
printRow i st = do
  let t    = fromIntegral i * dt
      x1   = lookupPos  (AtomicC "m1") st
      x2   = lookupPos  (AtomicC "m2") st
      p1   = lookupMom  (AtomicC "m1") st
      p2   = lookupMom  (AtomicC "m2") st
      ptot = totalMomentum st
      e    = totalEnergy st
      e0   = totalEnergy initState
      erel = (e - e0) / abs e0
  putStrLn $ concat
    [ show t,   ","
    , show x1,  ","
    , show x2,  ","
    , show p1,  ","
    , show p2,  ","
    , show ptot,","
    , show e,   ","
    , show erel
    ]

main :: IO ()
main = do
  putStrLn "t,x1,x2,p1,p2,Ptot,E,Erel"
  mapM_ (uncurry printRow)
        (take (nSteps + 1) (zip [0..] trajectory))
