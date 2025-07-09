module EulerNRSpec (spec) where

import Test.Hspec
import Components                 (Component(AtomicC))
import NState                     (emptyNS, insertPos, insertMom, lookupPos)
import NumericWorkflow            (applyNumericWorkflow)
import Physics.Integrators.EulerNR            (eulerNR)

spec :: Spec
spec = describe "Euler numeric integrator" $ do

  it "bodies move toward each other after one step" $ do
    let c1     = AtomicC "1"
        c2     = AtomicC "2"
        masses = [(c1, 1.0), (c2, 1.0)]
        dt     = 0.1
        g      = 1.0

        workflow = eulerNR dt g masses

        start = insertPos c1 (-1)
              $ insertPos c2   1
              $ insertMom c1   0
              $ insertMom c2   0
              $ emptyNS

        end  = applyNumericWorkflow workflow dt start
        q1'  = lookupPos c1 end
        q2'  = lookupPos c2 end

    q1' `shouldSatisfy` (> (-1))
    q2' `shouldSatisfy` (<   1)
