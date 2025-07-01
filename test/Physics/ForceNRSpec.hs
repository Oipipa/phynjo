{-# LANGUAGE OverloadedStrings #-}
module Physics.ForceNRSpec (spec) where

import Test.Hspec
import Physics.ForceNR       (forceNR)
import Physics.Force         (Force(..))
import NState                ( emptyNS
                             , insertPos, insertMom
                             , lookupPos, lookupMom )
import NumericRule           (applyNumericRule, nrDomain)
import Components            (Component(AtomicC))
import qualified Data.Set     as S

spec :: Spec
spec = describe "Physics.ForceNR.forceNR" $ do

  let dt = 1.0

  describe "Drag" $ do
    let c      = AtomicC "x"
        masses = [(c,2.0)]               -- m = 2kg
        f      = Drag 0.2                -- γ = 0.2
        rule   = forceNR f masses
        st0    = insertMom c 10 emptyNS
        st1    = applyNumericRule rule dt st0
        -- F = –γ·(p/m) = –0.2*(10/2) = –1
        -- Δp = dt*F = –1 ⇒ p' = 9
        expectedP = 9.0

    it "domain includes the component" $
      nrDomain rule `shouldBe` S.singleton c

    it "updates momentum by -γ·(p/m)·dt" $
      lookupMom c st1 `shouldBe` expectedP

  describe "Spring" $ do
    let i       = AtomicC "i"
        j       = AtomicC "j"
        rest    = 1.0
        k       = 5.0
        masses  = [(i,1.0),(j,1.0)]
        -- positions: i@0, j@2 ⇒ dx=2 ⇒ disp=2-1=1
        stPos0  = insertPos i 0
                $ insertPos j (2*rest) emptyNS
        stMom0  = insertMom i 0
                $ insertMom j 0 stPos0
        f       = Spring i j k rest
        rule    = forceNR f masses
        st1     = applyNumericRule rule dt stMom0
        expectedPi =  k * (2 - rest)  -- 5 * 1 = 5
        expectedPj = -expectedPi

    it "domain includes both bodies" $
      nrDomain rule `shouldBe` S.fromList [i,j]

    it "updates i momentum by +k*(dx-rest)*dt" $
      lookupMom i st1 `shouldBe` expectedPi

    it "updates j momentum by –k*(dx-rest)*dt" $
      lookupMom j st1 `shouldBe` expectedPj
