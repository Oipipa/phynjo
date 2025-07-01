{-# LANGUAGE OverloadedStrings #-}

module SymbolicPhysics.SRuneSpec (spec) where

import Test.Hspec
import qualified Data.Set as S
import Components (Component(..))
import SymbolicPhysics.SymbolicD
import SymbolicPhysics.SState
import SymbolicPhysics.SRune

spec :: Spec
spec = describe "SymbolicPhysics.SRune" $ do

  let c = AtomicC "particle"
      dtVar = var "dt"
      state0 = insertS c (var "x0") emptySState
      -- A rune for x' = x + v*dt, using variable v
      rune = SR
        { domainS = S.fromList [c]
        , stepS = \dt st ->
            let x = lookupS c st
                v = var "v"
            in insertS c (add x (mul v dt)) st
        }

  describe "applySRuneWorld" $ do
    it "applies a simple symbolic update" $ do
      let st1 = applySRuneWorld rune dtVar state0
      lookupS c st1 `shouldBe` add (var "x0") (mul (var "v") (var "dt"))

    it "works with numeric dt" $ do
      let st2 = applySRuneWorld rune (constant 0.5) state0
      lookupS c st2 `shouldBe` add (var "x0") (mul (var "v") (constant 0.5))

    it "does nothing outside domain" $ do
      -- define a state for a different component, should remain unchanged
      let d = AtomicC "other"
          runeOther = SR
            { domainS = S.fromList [d]
            , stepS = \dt st ->
                let x = lookupS d st
                in insertS d (add x dt) st
            }
          stOther0 = insertS d (var "y0") emptySState
          stOther1 = applySRuneWorld runeOther (constant 1) stOther0
      lookupS d stOther1 `shouldBe` add (var "y0") (constant 1)

    it "can update multiple components" $ do
      -- a rune that touches two components
      let c2 = AtomicC "p2"
          rune2 = SR
            { domainS = S.fromList [c, c2]
            , stepS = \dt st ->
                let x1 = lookupS c st
                    x2 = lookupS c2 st
                in insertS c2 (add x2 dt) $ insertS c (add x1 dt) st
            }
          st0 = insertS c (var "a") $ insertS c2 (var "b") emptySState
          st1 = applySRuneWorld rune2 (constant 1) st0
      lookupS c st1  `shouldBe` add (var "a") (constant 1)
      lookupS c2 st1 `shouldBe` add (var "b") (constant 1)
