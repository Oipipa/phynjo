module DriftNRSpec (spec) where

import Test.Hspec
import Base.Components      (Component(..))
import Base.NState          (emptyNS, insertPos, insertMom, lookupPos)
import Base.NumericRule     (applyNumericRule)
import Physics.DriftNR (driftNR)

spec :: Spec
spec = describe "Numeric drift rule" $ do
  let c     = AtomicC "ball"
      m     = 2.0 
      dt    = 0.1
      rule  = driftNR [(c,m)]

      start = insertMom c 10 
            $ insertPos c 0 
            $ emptyNS

      -- apply one step
      end  = applyNumericRule rule dt start
      qEnd = lookupPos c end

  it "updates position by dt·p/m" $
    qEnd `shouldBe` (0 + dt * 10 / m)
