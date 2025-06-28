{-# LANGUAGE OverloadedStrings #-}
module SpellSpec (spec) where

import Test.Hspec
import qualified Data.Set as Set

import Literal       (emptyLiteral, literalFromList)
import Transition    (singletonPhen, epsilon)
import Rune          (Rune(..), toAction)
import Spell         (Spell(..), applySpellWorld, applySpellPhen)
import Components    (Component(AtomicC))

spec :: Spec
spec = describe "Spell module" $ do

  let t0 = 0
      lit0 = emptyLiteral
      a = AtomicC "a"

      -- no‐op rune: empty domain, identity f and no g
      noOpRune = Rune
        { domainRune = Set.empty
        , fRune      = id
        , gRune      = \_ _ -> Set.empty
        }

      -- phen‐only rune on 'a'
      phenRune = Rune
        { domainRune = Set.fromList [a]
        , fRune      = id
        , gRune      = \t _ -> singletonPhen (epsilon a t)
        }

      sNoOp = SRun noOpRune
      sPhen = SRun phenRune

  it "SRun noOp advances tick and leaves literal" $ do
    applySpellWorld sNoOp t0 lit0 `shouldBe` (t0+1, lit0)
    applySpellPhen  sNoOp t0 lit0 `shouldBe` Set.empty

  it "SRun phenRune emits a phen but leaves state" $ do
    let (t1, l1) = applySpellWorld sPhen t0 lit0
    t1 `shouldBe` t0+1
    l1 `shouldBe` lit0
    applySpellPhen sPhen t0 lit0
      `shouldBe` singletonPhen (epsilon a t0)

  it "SSeq composes two runs" $ do
    let s2 = SSeq sNoOp sPhen
        (t2, l2) = applySpellWorld s2 t0 lit0
    t2 `shouldBe` t0+2
    applySpellPhen s2 t0 lit0
      `shouldBe` singletonPhen (epsilon a (t0+1))

  it "SPar runs two spells in parallel" $ do
    let sPar = SPar sPhen sPhen
        (tPar, _) = applySpellWorld sPar t0 lit0
    tPar `shouldBe` t0+1
    applySpellPhen sPar t0 lit0
      `shouldBe`
      Set.union
        (singletonPhen (epsilon a t0))
        (singletonPhen (epsilon a t0))

  it "Nested SSeq and SPar works" $ do
    let complex = SSeq (SPar sNoOp sPhen) sNoOp
    applySpellPhen complex t0 lit0
      `shouldBe` singletonPhen (epsilon a t0)
