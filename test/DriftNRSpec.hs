module DriftNRSpec (spec) where

import Test.Hspec
import Components            (Component(..))
import NState
import NRune          (applyNRuneWorld)
import Physics.DriftNR (driftNR)

spec :: Spec
spec = describe "Numeric drift rune" $ do
  let c    = AtomicC "ball"
      m    = 2.0                -- kg
      dt   = 0.1
      rune = driftNR [(c,m)]

      start = insertMom c 10     -- p = 10 kg·m/s
            $ insertPos c  0     -- q = 0 m
            $ emptyNS

      end   = applyNRuneWorld rune dt start
      qEnd  = lookupPos c end

  it "updates position by dt·p/m" $
       qEnd `shouldBe` (0 + dt * 10 / m)
