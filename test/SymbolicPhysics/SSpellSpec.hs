{-# LANGUAGE OverloadedStrings #-}

module SymbolicPhysics.SSpellSpec (spec) where

import Test.Hspec
import qualified Data.Set as S
import Components (Component(..))
import SymbolicPhysics.SymbolicD
import SymbolicPhysics.SState
import SymbolicPhysics.SRune
import SymbolicPhysics.SSpell
import Control.Exception (evaluate)

spec :: Spec
spec = describe "SymbolicPhysics.SSpell" $ do
  let c1 = AtomicC "a"
      c2 = AtomicC "b"
      dt = var "dt"
      st0 = insertS c1 (var "x") $ insertS c2 (var "y") emptySState

      rune1 = SR { domainS = S.singleton c1
                 , stepS = \dt st ->
                     let x = lookupS c1 st
                     in insertS c1 (add x dt) st
                 }
      rune2 = SR { domainS = S.singleton c2
                 , stepS = \dt st ->
                     let y = lookupS c2 st
                     in insertS c2 (sub y dt) st
                 }

      spell1 = SRun rune1
      spell2 = SRun rune2

  describe "applySSpellWorld - sequential composition" $ do
    it "applies two spells in sequence" $ do
      let seqSpell = SSeq spell1 spell2
          st1 = applySSpellWorld seqSpell (constant 1) st0
      lookupS c1 st1 `shouldBe` add (var "x") (constant 1)
      lookupS c2 st1 `shouldBe` sub (var "y") (constant 1)

  describe "applySSpellWorld - parallel composition" $ do
    it "applies two spells in parallel when domains are disjoint" $ do
      let parSpell = SPar spell1 spell2
          st1 = applySSpellWorld parSpell (constant 2) st0
      lookupS c1 st1 `shouldBe` add (var "x") (constant 2)
      lookupS c2 st1 `shouldBe` sub (var "y") (constant 2)

    it "errors when domains overlap" $ do
      let overlapping = SPar spell1 spell1 -- both touch c1
      (evaluate (applySSpellWorld overlapping (constant 1) st0))
        `shouldThrow` anyErrorCall

  describe "domainSpell" $ do
    it "returns all touched components" $ do
      let s = SPar spell1 spell2
      domainSpell s `shouldBe` S.fromList [c1, c2]
