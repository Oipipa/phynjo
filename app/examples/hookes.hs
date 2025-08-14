-- 1D 2 body Hooke's oscillator
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Phynjo.Core 
import Phynjo.Forces

import qualified Numeric.Units.Dimensional.Prelude as D
import qualified Data.Map.Strict       as M

-- | Simulation parameters
dt      :: Double
dt       = 0.01
nSteps  :: Int
nSteps   = 2000

-- | Hooke spring parameters
kSpring :: Double
kSpring  = 10.0
restLen :: Double
restLen   = 1.0

-- | Two 1 kg bodies at x=0 and x=restLen+0.1
body1, body2 :: Body
body1 = mkBody "m1" (1 *~ kilo gram) 0.0
body2 = mkBody "m2" (1 *~ kilo gram) (restLen + 0.1)

-- | Extended System
sys :: System
sys = mkSystem [body1, body2]

-- | Initial NState from positions only (momenta default to 0)
initState :: NState
initState =
  let posUnitMap = getLiteral (sPos sys)   -- Component -> Quantity DLength Double
      posMap     = M.map (\q -> q /~ metre) posUnitMap
      st0        = emptyNS
  in  M.foldrWithKey insertPos st0 posMap

forces :: [Force]
forces = [ Spring (AtomicC "m1") (AtomicC "m2") kSpring restLen ]

-- | Leapfrog‐based numeric spell
spell :: NumericWorkflow
spell = addForces dt forces sys

-- | Trajectory of states
trajectory :: [NState]
trajectory = iterate (applyNumericWorkflow spell dt) initState

-- | Total momentum p1 + p2
totalMomentum :: NState -> Double
totalMomentum st =
    lookupMom (AtomicC "m1") st
  + lookupMom (AtomicC "m2") st

-- | Total energy: kinetic + spring potential
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

-- | Print CSV: t,x1,x2,p1,p2,Ptot,E,Erel
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
    , show e   , ","
    , show erel
    ]

main :: IO ()
main = do
  putStrLn "t,x1,x2,p1,p2,Ptot,E,Erel"
  mapM_ (uncurry printRow)
        (take (nSteps + 1) (zip [0..] trajectory))