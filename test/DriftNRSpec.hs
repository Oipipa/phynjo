module DriftNRSpec (spec) where

import Test.Hspec
import Components      (Component(..))
import NState          (emptyNS, insertPos, insertMom, lookupPos)
import NumericRule     (applyNumericRule)
import Physics.DriftNR (driftNR)

spec :: Spec
spec = describe "Numeric drift rule" $ do
  let c     = AtomicC "ball"
      m     = 2.0               -- kg
      dt    = 0.1
      rule  = driftNR [(c,m)]

      -- start: q = 0 m, p = 10 kg·m/s
      start = insertMom c 10    -- mom first (p = 10)
            $ insertPos c 0     -- pos next (q = 0)
            $ emptyNS

      -- apply one step
      end  = applyNumericRule rule dt start
      qEnd = lookupPos c end

  it "updates position by dt·p/m" $
    qEnd `shouldBe` (0 + dt * 10 / m)
