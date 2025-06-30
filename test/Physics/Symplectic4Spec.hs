{-# LANGUAGE OverloadedStrings #-}

module Physics.Symplectic4Spec (spec) where

import Test.Hspec
import Physics.Symplectic4 (symplectic4)
import Physics.Force       (Force(..))
import Components          (Component(AtomicC))
import NSpell               (domainS, applyNSpellWorld)
import NState ( NState
              , emptyNS
              , insertPos, insertMom
              , lookupPos, lookupMom )
import qualified Data.Set     as S

spec :: Spec
spec = describe "Physics.Symplectic4.symplectic4" $ do
  let i      = AtomicC "i"
      j      = AtomicC "j"
      masses = [(i,1.0),(j,1.0)]
      rest   = 2.0
      k      = 1.0
      f      = Spring i j k rest
      dt     = 0.5

      spell0 = symplectic4 0 masses f
      spell  = symplectic4 dt masses f

      -- initial state: bodies at rest-length, zero momentum
      state0 :: NState
      state0 = insertMom i 0 $
               insertMom j 0 $
               insertPos i 0 $
               insertPos j rest emptyNS

      state1 = applyNSpellWorld spell dt state0
      state0' = applyNSpellWorld spell0 0 state0

  it "domain includes both bodies" $
    domainS spell `shouldBe` S.fromList [i,j]

  it "dt = 0 leaves the state unchanged" $
    state0' `shouldBe` state0

  it "at rest stays at rest (positions unchanged)" $ do
    lookupPos i state1 `shouldBe` 0
    lookupPos j state1 `shouldBe` rest

  it "at rest stays at rest (momenta unchanged)" $ do
    lookupMom i state1 `shouldBe` 0
    lookupMom j state1 `shouldBe` 0
