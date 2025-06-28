{-# LANGUAGE OverloadedStrings #-}
module ConstraintSpec (spec) where

import Test.Hspec
import qualified Data.Set as Set

import Literal    (emptyLiteral, getLiteral, literalFromList)
import Constraint (Invariant, preservesInvariant)
import Rune       (Rune(..))
import Spell      (Spell(SRun))
import Components (Component(AtomicC))

spec :: Spec
spec = describe "Constraint module" $ do

  let t0 = 0
      a  = AtomicC "a"

      -- no-op rune (does nothing)
      noOpRune = Rune
        { domainRune = Set.empty
        , fRune      = id
        , gRune      = \_ _ -> Set.empty
        }
      noOpSpell = SRun noOpRune

      -- a simple rune that flips presence of 'a'
      flipRune = Rune
        { domainRune = Set.singleton a
        , fRune      = \lit ->
            let s = getLiteral lit
            in if Set.member a s
                 then literalFromList []
                 else literalFromList [a]
        , gRune      = \_ _ -> Set.empty
        }
      flipSpell = SRun flipRune

      -- invariants
      alwaysTrue :: Invariant
      alwaysTrue _ = True

      -- holds only on emptyLiteral
      isEmpty :: Invariant
      isEmpty lit = getLiteral lit == Set.empty

  it "alwaysTrue is preserved by any spell" $ do
    preservesInvariant noOpSpell t0 emptyLiteral alwaysTrue
      `shouldBe` True
    preservesInvariant flipSpell t0 emptyLiteral alwaysTrue
      `shouldBe` True

  it "fails if invariant false initially" $ do
    let neverTrue :: Invariant
        neverTrue _ = False
    preservesInvariant noOpSpell t0 emptyLiteral neverTrue
      `shouldBe` False

  it "fails if invariant false after flip" $ do
    -- initial isEmpty holds, but flipSpell breaks it
    preservesInvariant flipSpell t0 emptyLiteral isEmpty
      `shouldBe` False
