{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Literal        (Literal, emptyLiteral, literalFromList, getLiteral)
import           Components     (Component(AtomicC))
import           Rune           (Rune(..))
import           Spell          (Spell(..), applySpellWorld, applySpellPhen)
import           Transition     (Phenomenon, epsilon)
import           Text.Printf    (printf)
import qualified Data.Set       as Set

-- | When input contains "Fall" ⇒ enter contact.
fallRune :: Rune
fallRune = Rune
  { domainRune = Set.fromList [AtomicC "Fall", AtomicC "Contact"]
  , fRune      = \lit ->
      if Set.member (AtomicC "Fall") (getLiteral lit)
        then literalFromList [AtomicC "Contact"]
        else lit
  , gRune      = \t lit ->
      if Set.member (AtomicC "Fall") (getLiteral lit)
        then Set.singleton (epsilon (AtomicC "Contact") t)
        else Set.empty
  }

-- | When input contains "Rise" ⇒ leave contact.
riseRune :: Rune
riseRune = Rune
  { domainRune = Set.fromList [AtomicC "Rise", AtomicC "Contact"]
  , fRune      = \lit ->
      if Set.member (AtomicC "Rise") (getLiteral lit)
        then emptyLiteral
        else lit
  , gRune      = \t lit ->
      if Set.member (AtomicC "Rise") (getLiteral lit)
        then Set.singleton (epsilon (AtomicC "Contact") t)
        else Set.empty
  }

-- | Sequence: on each tick, first run fallRune, then run riseRune
bounceFSM :: Spell
bounceFSM = SSeq (SRun fallRune) (SRun riseRune)

----------------------------------------------------------------------  
-- 1D bounce‐under‐gravity, hooked to the boolean FSM
----------------------------------------------------------------------  

g, dt, tmax, e :: Double
g    = 9.81    -- gravity
dt   = 0.01    -- time step
tmax = 10.0    -- total sim time
e    = 0.9     -- restitution coefficient

-- | Simulate from initial (y,v), threading an Int tick and the FSM literal.
simulate :: IO ()
simulate = go 0 0 emptyLiteral y0 v0
  where
    y0 = 10.0
    v0 =  0.0

    -- now tLit :: Int, not Double
    go :: Double   -- physical time
       -> Int      -- tick counter for the FSM
       -> Literal  -- FSM world‐literal
       -> Double   -- position y
       -> Double   -- velocity v
       -> IO ()
    go !t _   _   _ _   | t > tmax = return ()
    go !t !tLit !lit !y !v = do
      -- 1) numeric Euler step
      let v1 = v - g * dt
          y1 = y + v1 * dt

      -- 2) build input literal: "Fall" if below ground, else "Rise"
      let inp = if y1 <= 0
                  then literalFromList [AtomicC "Fall"]
                  else literalFromList [AtomicC "Rise"]

      -- 3) run the FSM two‐stage Spell in one go
      let (tLit', lit') = applySpellWorld bounceFSM tLit inp
          ph            = applySpellPhen  bounceFSM tLit inp

      -- 4) on any Contact‐phenomenon, flip & damp velocity, reflect height
      let isBounce = not (Set.null ph)
          v2 = if isBounce then -e * v1 else v1
          y2 = if isBounce then abs y1      else y1

      -- 5) emit CSV
      printf "%.3f,%.4f,%.4f,%5s,%s\n"
        t y v
        (show (Set.member (AtomicC "Contact") (getLiteral lit')))
        (show ph)

      go (t + dt) tLit' lit' y2 v2

main :: IO ()
main = do
  putStrLn "time,y,v,contact,phenomena"
  simulate
