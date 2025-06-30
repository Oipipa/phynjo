-- 1D two body
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

-- | Simulation parameters
dt     :: Double
dt      = 0.001

nSteps :: Int
nSteps   = 8000

-- | Physical constants
gConst :: Double
gConst   = 1   -- gravitational constant (m³·kg⁻¹·s⁻²)

-- | Two bodies, m1 and m2
m1, m2 :: Double
m1      = 5.0            -- kg
m2      = 1.0            -- kg

-- | Initial positions (m)
x1_0, x2_0 :: Double
x1_0    = 0.0
x2_0    = 10.0

-- | Build our bodies
body1, body2 :: Body
body1   = mkBody "m1" (m1 *~ kilo gram) x1_0
body2   = mkBody "m2" (m2 *~ kilo gram) x2_0

-- | Extended system (unit‐checked + raw masses)
sys :: System
sys     = mkSystem [body1, body2]

-- | Initial numeric state: set positions, zero momenta
initState :: NState
initState =
  let posUnitMap = getLiteral (sPos sys)        -- Component -> Quantity DLength Double
      posMap     = M.map (\q -> q /~ metre) posUnitMap
      st0        = emptyNS
  in  M.foldrWithKey insertPos st0 posMap

-- | Use pure Newtonian gravity between the two bodies
forces :: [Force]
forces  = [Gravity gConst]

-- | Leapfrog‐based integrator
spell :: NSpell
spell   = addForces dt forces sys

-- | Trajectory of NStates
trajectory :: [NState]
trajectory = iterate (applyNSpellWorld spell dt) initState

-- | Total momentum p1 + p2
totalMomentum :: NState -> Double
totalMomentum st =
    lookupMom (AtomicC "m1") st
  + lookupMom (AtomicC "m2") st

-- | Total energy: kinetic + gravitational potential
totalEnergy :: NState -> Double
totalEnergy st =
  let -- raw masses
      m1' = sMassMap sys ! AtomicC "m1"
      m2' = sMassMap sys ! AtomicC "m2"
      -- positions
      x1  = lookupPos (AtomicC "m1") st
      x2  = lookupPos (AtomicC "m2") st
      -- momenta & velocities
      p1  = lookupMom (AtomicC "m1") st
      p2  = lookupMom (AtomicC "m2") st
      v1  = p1 / m1'
      v2  = p2 / m2'
      -- kinetic energies
      ke1 = 0.5 * m1' * v1 * v1
      ke2 = 0.5 * m2' * v2 * v2
      -- potential energy (–G m₁ m₂ / |x₂–x₁|)
      r   = abs (x2 - x1)
      pe  = - gConst * m1' * m2' / r
  in ke1 + ke2 + pe

-- | Print CSV row: t, x1, x2, p1, p2, Ptot, E, ΔE/E₀
printRow :: Int -> NState -> IO ()
printRow i st = do
  let t     = fromIntegral i * dt
      x1    = lookupPos  (AtomicC "m1") st
      x2    = lookupPos  (AtomicC "m2") st
      p1    = lookupMom  (AtomicC "m1") st
      p2    = lookupMom  (AtomicC "m2") st
      ptot  = totalMomentum st
      e     = totalEnergy st
      e0    = totalEnergy initState
      erel  = (e - e0) / abs e0
  putStrLn $ concat
    [ show t,   ","
    , show x1,  ","
    , show x2,  ","
    , show p1,  ","
    , show p2,  ","
    , show ptot, ","
    , show e,   ","
    , show erel
    ]

main :: IO ()
main = do
  putStrLn "t,x1,x2,p1,p2,Ptot,E,Erel"
  mapM_ (uncurry printRow)
        $ take (nSteps + 1) (zip [0..] trajectory)
